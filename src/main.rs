#[derive(PartialEq, Eq, Copy, Clone)]
enum Reg {
    Rax,
    Rbx,
    Rcx,
    Rdx,
    Rsp,
    Rbp,
    Rsi,
    Rdi,
    R8,
    R9,
    R10,
    R11,
    R12,
    R13,
    R14,
    R15,
}

#[derive(Copy, Clone)]
enum Operand {
    Reg(Reg),
    Imm(i64),
    Mem()
}


#[derive(Default)]
struct Assembler {
    bytes: Vec<u8>,
}

fn reg_encoding(reg: Reg) -> (bool, u8) {
    match reg {
        Reg::Rax => (false, 0b000),
        Reg::Rbx => (false, 0b011),
        Reg::Rcx => (false, 0b001),
        Reg::Rdx => (false, 0b010),
        Reg::Rsp => (false, 0b100),
        Reg::Rbp => (false, 0b101),
        Reg::Rsi => (false, 0b110),
        Reg::Rdi => (false, 0b111),
        Reg::R8  => (true,  0b000),
        Reg::R9  => (true,  0b001),
        Reg::R10 => (true,  0b010),
        Reg::R11 => (true,  0b011),
        Reg::R12 => (true,  0b100),
        Reg::R13 => (true,  0b101),
        Reg::R14 => (true,  0b110),
        Reg::R15 => (true,  0b111),
    }
}

#[derive(Default)]
struct Op<'a> {
    opcode: &'a [u8],
}
use std::convert::TryFrom;

type Mem = (Option<Reg>, Option<(Reg, usize)>, Option<i64>);

struct Encoding<'a> {
    regreg: Option<Op<'a>>,
    regmem: Option<Op<'a>>,
    memreg: Option<Op<'a>>,
}

macro_rules! make_instruction {
    ($name: tt, $encoding: tt) => {
        impl Assembler {
            fn $name(operands: &[Operand]) {
                let encoding = $encoding;

                match operands {
                    &[Operand::Reg(reg), Operand::Reg(reg)] => {

                    }
                }
            }
        }
    }
}

make_instruction! {
    mov,
    Encoding {
        regreg: Some(Op { opcode: &[0x8b] }),
        ..Encoding::default()
    }
}

impl Assembler {
    fn push(&mut self, bytes: &[u8]) {
        self.bytes.extend_from_slice(bytes);
    }

    fn encode_regreg(&mut self, reg1: Reg, reg2: Reg, op: &Op) {
        let (reg1_e, reg1_enc) = reg_encoding(reg1);
        let (reg2_e, reg2_enc) = reg_encoding(reg2);

        self.rex(true, false, reg1_e, reg2_e);
        self.push(&op.opcode);
        self.modrm(0b00, reg1_enc, reg2_enc);
    }

    fn rex(&mut self, w: bool, r: bool, x: bool, b: bool) {
        let w = w as u8;
        let r = r as u8;
        let x = x as u8;
        let b = b as u8;

        self.push(&[0b0100 << 4 | w << 3 | r << 2 | x << 1 | b]);
    }

    fn modrm(&mut self, mod_: u8, reg: u8, rm: u8) {
        assert!(mod_ <= 0b11, "Mod in modrm is too big.");
        assert!(reg <= 0b111, "Reg in modrm is too big.");
        assert!(rm <= 0b111,  "Rm in modrm is too big.");

        self.push(&[mod_ << 6 | reg << 3 | rm]);
    }

    fn sib(&mut self, base: u8, index: u8, scale: u8) {
        assert!(base <= 0b111, "Base in sib is too big.");
        assert!(index <= 0b111, "Index in sib is too big.");
        assert!(scale <= 0b11, "Scale in sib is too big.");

        self.push(&[scale << 6 | index << 3 | base]);
    }

    fn push32(&mut self, value: u32) {
        self.push(&value.to_le_bytes());
    }

    fn regreg(&mut self, opcode: &[u8], reg1: Reg, reg2: Reg) {
        let (reg1_e, reg1_enc) = reg_encoding(reg1);
        let (reg2_e, reg2_enc) = reg_encoding(reg2);

        self.rex(true, reg2_e, false, reg1_e);
        self.push(opcode);
        self.modrm(0b11, reg2_enc, reg1_enc);
    }

    fn mov(&mut self, operands: &[Operand]) {
        match operands {
            &[Operand::Reg(reg), Operand::Imm(imm)] => {
                let (x, enc) = reg_encoding(reg);
                self.rex(true, false, false, x);
                self.push(&[0xc7]);
                self.modrm(0b11, 0, enc);
                self.push32(imm as u32);
            }
            &[Operand::Reg(reg1), Operand::Reg(reg2)] => {
                self.regreg(&[0x89], reg1, reg2);
            }
            _ => panic!(),
        }
    }


    /*
    fn test(&mut self) {
        type Mem = (Option<Reg>, Option<(Reg, usize)>, Option<i64>);

        let mut mem: Mem = (Some(Reg::R13), None, None);

        let mut require_sib = false;
        let mut has_base = false;

        if let Some(reg) = mem.0 {
            has_base = true;

            let base_enc = reg_encoding(reg).1;

            if base_enc == 0b100 || base_enc == 0b101 {
                require_sib = true;
                if base_enc == 0b101 && mem.2.is_none() {
                    mem.2 = Some(0);
                }
            }
        }

        let value = Reg::Rsp;

        let rex_r = reg_encoding(value).0;
        let rex_x = mem.1.map(|reg| reg_encoding(reg.0).0).unwrap_or(false);
        let rex_b = mem.0.map(|reg| reg_encoding(reg).0).unwrap_or(false);

        self.rex(true, rex_r, rex_x, rex_b);
        self.push(&[0x89]);

        if let Some(reg) = mem.1 {
            require_sib = true;
            assert!(mem.0.is_some());
        }


        let mut has_displacement = mem.2.is_some();

        if has_displacement && !has_base {
            self.modrm(0b00, reg_encoding(value).1, 0b100);
            self.push(&[0x25]);
            self.push(&(mem.2.unwrap() as u32).to_le_bytes());
        } else if require_sib {
            assert!(has_base);

            if has_displacement {
                self.modrm(0b10, reg_encoding(value).1, 0b100);
            } else {
                self.modrm(0b00, reg_encoding(value).1, 0b100);
            }

            if let Some((index, _)) = mem.1 {
                assert!(index != Reg::Rsp);
            }

            let base = mem.0.unwrap();
            let index = mem.1.unwrap_or((Reg::Rsp, 1));

            let scale = match index.1 {
                1 => 0,
                2 => 1,
                4 => 2,
                8 => 3,
                _ => panic!(),
            };

            let sib = reg_encoding(base).1 | (reg_encoding(index.0).1 << 3) | (scale << 6);
            self.push(&[sib]);

            if has_displacement {
                self.push(&((mem.2.unwrap() as u32).to_le_bytes()));
            }
        } else {
            self.modrm(0b00, reg_encoding(value).1, reg_encoding(mem.0.unwrap()).1);
        }



        /*
        self.modrm(0b00, reg_encoding(value).1, 0b100);
        self.push(&[0x25]);
        */

        /*
        self.push(&[5]);
        self.push(&[0x11, 0x22, 0x33, 0x44]);
        */



        let base = Reg::Rcx;
        let index = Reg::Rdx;
        let scale = 1;

        let has_displacement = true;

        /*
        self.rex(true, false, false, false);
        self.push(&[0x89]);
        self.modrm(0b10, reg_encoding(value).1, reg_encoding(base).1);
        self.push(&[0x18, 0x00, 0x00, 0x00]);
        */

        /*
        self.modrm(0b10, reg_encoding(value).1, 0b100);
        self.push(&[0x19]);
        self.push(&[0x18, 0x00, 0x00, 0x00]);
        */
    }
    */

    fn is_rex(prefix: u8) -> bool {
        prefix & (0b1111 << 4) == (0b0100 << 4)
    }

    fn encode_memory_operand(
        &mut self,
        regop:    u8,
        rex_r:    bool,
        rex_w:    bool,
        prefixes: &[u8],
        opcode:   &[u8],
        mut mem:  Mem,
    ) {
        let mut require_sib = false;
        let mut has_base    = false;

        const RM_SIB:  u8 = 0b100;
        const RM_DISP: u8 = 0b101;

        if let Some(base) = mem.0 {
            has_base = true;

            let encoding = reg_encoding(base).1;

            if encoding == RM_SIB {
                require_sib = true;
            }

            if encoding == RM_DISP && mem.2.is_none() {
                mem.2 = Some(0);
            }
        } else {
            assert!(mem.1.is_none(), "Memory operands without base cannot have index.");
            assert!(mem.2.is_some(), "Memory operands without base must have displacement.");
        }

        if let Some((index, _)) = mem.1 {
            assert!(index != Reg::Rsp, "Rsp cannot be used as an index.");
            require_sib = true;
        }

        if let Some(displacement) = mem.2 {
            if !(displacement >= i32::MIN as i64 && displacement <= i32::MAX as i64) {
                panic!("Displacement {} is too big.", displacement);
            }
        }

        let rex_b = mem.0.map(|reg| reg_encoding(reg).0).unwrap_or(false);
        let rex_x = mem.1.map(|reg| reg_encoding(reg.0).0).unwrap_or(false);

        for &prefix in prefixes {
            assert!(!Self::is_rex(prefix), "REX prefix is already included in prefix table.");
        }

        self.push(prefixes);
        self.rex(rex_w, rex_r, rex_x, rex_b);
        self.push(opcode);

        let has_displacement = mem.2.is_some();

        if !require_sib {
            if has_displacement {
                let displacement = mem.2.unwrap() as u32;

                if has_base {
                    self.modrm(0b10, regop, reg_encoding(mem.0.unwrap()).1);
                    self.push(&displacement.to_le_bytes());
                } else {
                    self.modrm(0b00, regop, RM_SIB);
                    self.sib(0b101, 0b100, 0);
                    self.push(&displacement.to_le_bytes());
                }
            } else {
                self.modrm(0b00, regop, reg_encoding(mem.0.unwrap()).1);
            }
        } else {
            if has_displacement {
                self.modrm(0b10, regop, RM_SIB);
            } else {
                self.modrm(0b00, regop, RM_SIB);
            }

            let base  = mem.0.unwrap();
            let index = mem.1.unwrap_or((Reg::Rsp, 1));

            let scale = match index.1 {
                1 => 0,
                2 => 1,
                4 => 2,
                8 => 3,
                _ => panic!("Invalid scale {}.", index.1),
            };

            self.sib(reg_encoding(base).1, reg_encoding(index.0).1, scale);

            if has_displacement {
                let displacement = mem.2.unwrap() as u32;
                self.push(&displacement.to_le_bytes());
            }
        }
    }

    fn test(&mut self) {
        type Mem = (Option<Reg>, Option<(Reg, usize)>, Option<i64>);

        let mut mem: Mem = (None, None, Some(0xcc));

        self.encode_memory_operand(0x1, false, true, &[], &[0x39], mem);
    }
}

fn main() {
    let mut asm = Assembler::default();

    use Operand::*;
    use crate::Reg::*;

    asm.mov(&[Reg(Rsp), Imm(10)]);
    asm.mov(&[Reg(Rbp), Reg(R15)]);
    asm.test();

    disasm(&asm);
}

fn disasm(asm: &Assembler) {
    std::fs::write("assembly.bin", &asm.bytes)
        .expect("Failed to write assembly.bin");

    std::process::Command::new("ndisasm")
        .args(&["-b", "64", "assembly.bin"])
        .spawn()
        .unwrap();
}
