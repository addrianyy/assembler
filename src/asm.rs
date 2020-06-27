#![allow(dead_code)]

use std::collections::HashMap;

#[derive(PartialEq, Eq, Copy, Clone)]
pub enum Reg {
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

type MemOperand = (Option<Reg>, Option<(Reg, usize)>, i64);

#[derive(Clone)]
pub enum Operand<'a> {
    Reg(Reg),
    Imm(i64),
    Mem(Option<Reg>, Option<(Reg, usize)>, i64),
    Rel(i32),
    Label(&'a str),
}

#[derive(Default)]
pub struct Assembler {
    pub bytes:       Vec<u8>,
    pub relocations: Vec<(String, usize, usize)>,
    pub labels:      HashMap<String, usize>,
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

#[derive(Copy, Clone)]
struct Op<'a> {
    opcode: &'a [u8],
}

#[derive(Copy, Clone, Default)]
struct Encoding<'a> {
    rex:        bool,
    regreg:     Option<Op<'a>>,
    regimm32:   Option<(Op<'a>, u8)>,
    memimm32:   Option<(Op<'a>, u8)>,
    reguimm8:   Option<(Op<'a>, u8)>,
    memuimm8:   Option<(Op<'a>, u8)>,
    regmem:     Option<Op<'a>>,
    memreg:     Option<Op<'a>>,
    regcl:      Option<(Op<'a>, u8)>,
    memcl:      Option<(Op<'a>, u8)>,
    reg:        Option<(Op<'a>, u8)>,
    mem:        Option<(Op<'a>, u8)>,
    rel32:      Option<Op<'a>>,
    imm32:      Option<Op<'a>>,
    uimm16:     Option<Op<'a>>,
    standalone: Option<Op<'a>>,
}

const DEFAULT_ENCODING: Encoding = Encoding {
    rex:        true,
    regreg:     None,
    regimm32:   None,
    memimm32:   None,
    reguimm8:   None,
    memuimm8:   None,
    regmem:     None,
    memreg:     None,
    regcl:      None,
    memcl:      None,
    reg:        None,
    mem:        None,
    rel32:      None,
    imm32:      None,
    uimm16:     None,
    standalone: None,
};

macro_rules! fits_within {
    ($value: expr, $type: tt) => {
        $value as i64 <= $type::MAX as i64 && $value as i64 >= $type::MIN as i64
    }
}

macro_rules! make_instruction {
    ($name: ident, $encoding: expr) => {
        impl Assembler {
            pub fn $name(&mut self, operands: &[Operand]) {
                const ENCODING: Encoding = $encoding;

                self.encode_instruction(operands, &ENCODING);
            }
        }
    }
}

make_instruction! {
    jmp,
    Encoding {
        rex:   false,
        rel32: Some(Op { opcode: &[0xe9] }),
        reg:   Some((Op { opcode: &[0xff] }, 4)),
        mem:   Some((Op { opcode: &[0xff] }, 4)),
        ..DEFAULT_ENCODING
    }
}

make_instruction! {
    call,
    Encoding {
        rex:   false,
        rel32: Some(Op { opcode: &[0xe8] }),
        reg:   Some((Op { opcode: &[0xff] }, 2)),
        mem:   Some((Op { opcode: &[0xff] }, 2)),
        ..DEFAULT_ENCODING
    }
}

make_instruction! {
    int3,
    Encoding {
        rex:        false,
        standalone: Some(Op { opcode: &[0xcc] }),
        ..DEFAULT_ENCODING
    }
}

make_instruction! {
    mov,
    Encoding {
        rex:      true,
        regreg:   Some(Op { opcode: &[0x8b] }),
        regmem:   Some(Op { opcode: &[0x8b]}),
        memreg:   Some(Op { opcode: &[0x89]}),
        regimm32: Some((Op { opcode: &[0xc7]}, 0)),
        memimm32: Some((Op { opcode: &[0xc7]}, 0)),
        ..DEFAULT_ENCODING
    }
}

macro_rules! simple_instruction {
    ($name: ident, $reg_regmem: expr, $memreg: expr, $regmem_imm32: expr, $reg: expr) => {
        make_instruction! {
            $name,
            Encoding {
                rex:      true,
                regreg:   Some(Op { opcode: &[$reg_regmem] }),
                regmem:   Some(Op { opcode: &[$reg_regmem]}),
                memreg:   Some(Op { opcode: &[$memreg]}),
                regimm32: Some((Op { opcode: &[$regmem_imm32]}, $reg)),
                memimm32: Some((Op { opcode: &[$regmem_imm32]}, $reg)),
                ..DEFAULT_ENCODING
            }
        }
    }
}

make_instruction! {
    push,
    Encoding {
        rex:   false,
        reg:   Some((Op { opcode: &[0xff]}, 6)),
        mem:   Some((Op { opcode: &[0xff]}, 6)),
        imm32: Some(Op { opcode: &[0x68] }),
        ..DEFAULT_ENCODING
    }
}

make_instruction! {
    ret,
    Encoding {
        uimm16:     Some(Op { opcode: &[0xc2] }),
        standalone: Some(Op { opcode: &[0xc3] }),
        ..DEFAULT_ENCODING
    }
}

make_instruction! {
    pop,
    Encoding {
        rex:   false,
        reg:   Some((Op { opcode: &[0x8f]}, 0)),
        mem:   Some((Op { opcode: &[0x8f]}, 0)),
        ..DEFAULT_ENCODING
    }
}

simple_instruction!(add, 0x03, 0x01, 0x81, 0);
simple_instruction!(sub, 0x2b, 0x29, 0x81, 5);
simple_instruction!(xor, 0x33, 0x31, 0x81, 6);
simple_instruction!(and, 0x23, 0x21, 0x81, 4);
simple_instruction!(or, 0x0b, 0x09, 0x81, 1);
simple_instruction!(cmp, 0x3b, 0x39, 0x81, 7);

macro_rules! shift_instruction {
    ($name: ident, $regmem_cl: expr, $regmem_imm8: expr) => {
        make_instruction! {
            $name,
            Encoding {
                rex:      true,
                regcl:    Some((Op { opcode: &[$regmem_cl.0] }, $regmem_cl.1)),
                memcl:    Some((Op { opcode: &[$regmem_cl.0] }, $regmem_cl.1)),
                reguimm8: Some((Op { opcode: &[$regmem_imm8.0] }, $regmem_imm8.1)),
                memuimm8: Some((Op { opcode: &[$regmem_imm8.0] }, $regmem_imm8.1)),
                ..DEFAULT_ENCODING
            }
        }
    }
}

shift_instruction!(shl, (0xd3, 4), (0xc1, 4));
shift_instruction!(shr, (0xd3, 5), (0xc1, 5));
shift_instruction!(sar, (0xd3, 7), (0xc1, 7));

macro_rules! make_cond {
    ($jcc_name: ident, $condmove_name: ident, $code: expr) => {
        make_instruction! {
            $jcc_name,
            Encoding {
                rex:   false,
                rel32: Some(Op { opcode: &[0x0f, $code] }),
                ..DEFAULT_ENCODING
            }
        }

        make_instruction! {
            $condmove_name,
            Encoding {
                rex:    true,
                regmem: Some(Op { opcode: &[0x0f, $code - 0x40] }),
                regreg: Some(Op { opcode: &[0x0f, $code - 0x40] }),
                ..DEFAULT_ENCODING
            }
        }
    }
}

make_cond!(ja, cmova, 0x87);
make_cond!(jae, cmovae, 0x83);
make_cond!(jb, cmovb, 0x82);
make_cond!(jbe, cmovbe, 0x86);
make_cond!(je, cmove, 0x84);
make_cond!(jg, cmovg, 0x8f);
make_cond!(jge, cmovge, 0x8d);
make_cond!(jl, cmovl, 0x8c);
make_cond!(jle, cmovle, 0x8e);
make_cond!(jna, cmovna, 0x86);
make_cond!(jnae, cmovnae, 0x82);
make_cond!(jnb, cmovnb, 0x83);
make_cond!(jnbe, cmovnbe, 0x87);
make_cond!(jnc, cmovnc, 0x83);
make_cond!(jne, cmovne, 0x85);
make_cond!(jng, cmovng, 0x8e);
make_cond!(jnge, cmovnge, 0x8c);
make_cond!(jnl, cmovnl, 0x8d);
make_cond!(jnle, cmovnle, 0x8f);
make_cond!(jnz, cmovnz, 0x85);
make_cond!(jz, cmovz, 0x84);

make_instruction! {
    mul,
    Encoding {
        rex: true,
        reg: Some((Op { opcode: &[0xf7] }, 4)),
        mem: Some((Op { opcode: &[0xf7] }, 4)),
        ..DEFAULT_ENCODING
    }
}

make_instruction! {
    imul,
    Encoding {
        rex:    true,
        reg:    Some((Op { opcode: &[0xf7] }, 5)),
        mem:    Some((Op { opcode: &[0xf7] }, 5)),
        regmem: Some(Op { opcode: &[0x0f, 0xaf] }),
        ..DEFAULT_ENCODING
    }
}

make_instruction! {
    idiv,
    Encoding {
        rex: true,
        reg: Some((Op { opcode: &[0xf7] }, 7)),
        mem: Some((Op { opcode: &[0xf7] }, 7)),
        ..DEFAULT_ENCODING
    }
}

make_instruction! {
    div,
    Encoding {
        rex: true,
        reg: Some((Op { opcode: &[0xf7] }, 6)),
        mem: Some((Op { opcode: &[0xf7] }, 6)),
        ..DEFAULT_ENCODING
    }
}

make_instruction! {
    dec,
    Encoding {
        rex: true,
        reg: Some((Op { opcode: &[0xff] }, 1)),
        mem: Some((Op { opcode: &[0xff] }, 1)),
        ..DEFAULT_ENCODING
    }
}

make_instruction! {
    inc,
    Encoding {
        rex: true,
        reg: Some((Op { opcode: &[0xff] }, 0)),
        mem: Some((Op { opcode: &[0xff] }, 0)),
        ..DEFAULT_ENCODING
    }
}

impl Assembler {
    fn encode_regreg(&mut self, reg1: Reg, reg2: Reg, op: &Op) {
        let (reg1_e, reg1_enc) = reg_encoding(reg1);
        let (reg2_e, reg2_enc) = reg_encoding(reg2);

        self.rex(true, reg1_e, false, reg2_e);
        self.push_code(&op.opcode);
        self.modrm(0b11, reg1_enc, reg2_enc);
    }

    fn encode_regimm(&mut self, reg: Reg, imm: i64, size: usize, op: &(Op, u8)) {
        let (reg_e, reg_enc) = reg_encoding(reg);

        self.rex(true, false, false, reg_e);
        self.push_code(&op.0.opcode);
        self.modrm(0b11, op.1, reg_enc);
        self.push_code(&imm.to_le_bytes()[..size]);
    }

    fn encode_memreg_regmem(&mut self, reg: Reg, mem: MemOperand, op: &Op) {
        let (reg_e, reg_enc) = reg_encoding(reg);

        self.encode_memory_operand(reg_enc, reg_e, true, &[], op.opcode, mem)
    }

    fn encode_memimm(&mut self, mem: MemOperand, imm: i64, size: usize, op: &(Op, u8)) {
        self.encode_memory_operand(op.1, false, true, &[], op.0.opcode, mem);
        self.push_code(&imm.to_le_bytes()[..size]);
    }

    fn encode_singlemem(&mut self, mem: MemOperand, op: &(Op, u8), encoding: &Encoding) {
        self.encode_memory_operand(op.1, false, encoding.rex, &[], op.0.opcode, mem);
    }

    fn encode_singlereg(&mut self, reg: Reg, op: &(Op, u8), encoding: &Encoding) {
        let (reg_e, reg_enc) = reg_encoding(reg);

        self.rex(encoding.rex, false, false, reg_e);

        self.push_code(&op.0.opcode);
        self.modrm(0b11, op.1, reg_enc);
    }

    fn encode_rel32(&mut self, rel: i32, label: Option<&str>, op: &Op, encoding: &Encoding) {
        let size_before = self.bytes.len();

        self.encode_imm32(rel, op, encoding);

        let inst_size = self.bytes.len() - size_before;

        if let Some(label) = label {
            self.relocations.push((label.to_string(), size_before, inst_size));
        }
    }

    fn encode_imm32(&mut self, imm: i32, op: &Op, encoding: &Encoding) {
        self.rex(encoding.rex, false, false, false);
        self.push_code(&op.opcode);
        self.push_code(&imm.to_le_bytes());
    }

    fn encode_uimm16(&mut self, imm: u16, op: &Op, encoding: &Encoding) {
        self.rex(encoding.rex, false, false, false);
        self.push_code(&op.opcode);
        self.push_code(&imm.to_le_bytes());
    }

    fn encode_standalone(&mut self, op: &Op, encoding: &Encoding) {
        self.rex(encoding.rex, false, false, false);
        self.push_code(&op.opcode);
    }

    fn encode_instruction(&mut self, operands: &[Operand], encoding: &Encoding) {
        match operands {
            &[] if  encoding.standalone.is_some() => {
                self.encode_standalone(encoding.standalone.as_ref().unwrap(), encoding);
            }
            &[Operand::Rel(rel)]
                if encoding.rel32.is_some() =>
            {
                self.encode_rel32(rel, None, encoding.rel32.as_ref().unwrap(), encoding);
            }
            &[Operand::Label(label)]
                if encoding.rel32.is_some() =>
            {
                self.encode_rel32(0, Some(label), encoding.rel32.as_ref().unwrap(), encoding);
            }
            &[Operand::Reg(reg)]
                if encoding.reg.is_some() =>
            {
                self.encode_singlereg(reg, encoding.reg.as_ref().unwrap(), encoding)
            }
            &[Operand::Mem(base, index, disp)]
                if encoding.mem.is_some() =>
            {
                self.encode_singlemem((base, index, disp),
                    encoding.mem.as_ref().unwrap(), encoding)
            }
            &[Operand::Imm(imm)]
                if fits_within!(imm, u16) && encoding.uimm16.is_some() =>
            {
                self.encode_uimm16(imm as u16, encoding.uimm16.as_ref().unwrap(), encoding);
            }
            &[Operand::Imm(imm)]
                if fits_within!(imm, i32) && encoding.imm32.is_some() =>
            {
                self.encode_imm32(imm as i32, encoding.imm32.as_ref().unwrap(), encoding);
            }
            &[Operand::Reg(reg), Operand::Reg(Reg::Rcx)]
                if encoding.regcl.is_some() =>
            {
                self.encode_singlereg(reg, encoding.regcl.as_ref().unwrap(), encoding);
            }
            &[Operand::Mem(base, index, disp), Operand::Reg(Reg::Rcx)]
                if encoding.memcl.is_some() =>
            {
                self.encode_singlemem((base, index, disp),
                    encoding.memcl.as_ref().unwrap(), encoding);
            }
            &[Operand::Reg(reg1), Operand::Reg(reg2)]
                if encoding.regreg.is_some() =>
            {
                self.encode_regreg(reg1, reg2, encoding.regreg.as_ref().unwrap());
            }
            &[Operand::Reg(reg), Operand::Imm(imm)]
                if fits_within!(imm, u8)  && encoding.reguimm8.is_some() =>
            {
                self.encode_regimm(reg, imm, 1, encoding.reguimm8.as_ref().unwrap());
            }
            &[Operand::Mem(base, index, disp), Operand::Imm(imm)]
                if fits_within!(imm, u8) && encoding.memuimm8.is_some() =>
            {
                self.encode_memimm((base, index, disp), imm, 1,
                    encoding.memuimm8.as_ref().unwrap());
            }
            &[Operand::Reg(reg), Operand::Imm(imm)]
                if fits_within!(imm, i32)  && encoding.regimm32.is_some() =>
            {
                self.encode_regimm(reg, imm, 4, encoding.regimm32.as_ref().unwrap());
            }
            &[Operand::Mem(base, index, disp), Operand::Imm(imm)]
                if fits_within!(imm, i32) && encoding.memimm32.is_some() =>
            {
                self.encode_memimm((base, index, disp),
                    imm, 4, encoding.memimm32.as_ref().unwrap());
            }
            &[Operand::Reg(reg), Operand::Mem(base, index, disp)]
                if encoding.regmem.is_some() =>
            {
                self.encode_memreg_regmem(reg, (base, index, disp),
                    encoding.regmem.as_ref().unwrap());
            }
            &[Operand::Mem(base, index, disp), Operand::Reg(reg)]
                if encoding.memreg.is_some() =>
            {
                self.encode_memreg_regmem(reg, (base, index, disp),
                    encoding.memreg.as_ref().unwrap());
            }
            _ => panic!(),
        }
    }

    fn push_code(&mut self, bytes: &[u8]) {
        self.bytes.extend_from_slice(bytes);
    }

    fn rex(&mut self, w: bool, r: bool, x: bool, b: bool) {
        if !w && !r && !x && !b {
            return;
        }

        let w = w as u8;
        let r = r as u8;
        let x = x as u8;
        let b = b as u8;

        self.push_code(&[0b0100 << 4 | w << 3 | r << 2 | x << 1 | b]);
    }

    fn modrm(&mut self, mod_: u8, reg: u8, rm: u8) {
        assert!(mod_ <= 0b11, "Mod in modrm is too big.");
        assert!(reg <= 0b111, "Reg in modrm is too big.");
        assert!(rm <= 0b111,  "Rm in modrm is too big.");

        self.push_code(&[mod_ << 6 | reg << 3 | rm]);
    }

    fn sib(&mut self, base: u8, index: u8, scale: u8) {
        assert!(base <= 0b111, "Base in sib is too big.");
        assert!(index <= 0b111, "Index in sib is too big.");
        assert!(scale <= 0b11, "Scale in sib is too big.");

        self.push_code(&[scale << 6 | index << 3 | base]);
    }

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
        mem:      MemOperand,
    ) {
        let mut require_sib = false;
        let mut has_base    = false;

        const RM_SIB:  u8 = 0b100;
        const RM_DISP: u8 = 0b101;

        let mut mem = if mem.2 == 0 {
            (mem.0, mem.1, None)
        } else {
            (mem.0, mem.1, Some(mem.2))
        };

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

        let mut byte_displacement = false;

        if let Some(displacement) = mem.2 {
            if !(displacement >= i32::MIN as i64 && displacement <= i32::MAX as i64) {
                panic!("Displacement {} is too big.", displacement);
            }

            if displacement >= i8::MIN as i64 && displacement <= i8::MAX as i64 {
                byte_displacement = true;
            }
        }

        let rex_b = mem.0.map(|reg| reg_encoding(reg).0).unwrap_or(false);
        let rex_x = mem.1.map(|reg| reg_encoding(reg.0).0).unwrap_or(false);

        for &prefix in prefixes {
            assert!(!Self::is_rex(prefix), "REX prefix is already included in prefix table.");
        }

        self.push_code(prefixes);
        self.rex(rex_w, rex_r, rex_x, rex_b);
        self.push_code(opcode);

        let has_displacement = mem.2.is_some();

        if !require_sib {
            if has_displacement {
                let displacement = mem.2.unwrap() as u32;

                if has_base {
                    self.modrm(0b10, regop, reg_encoding(mem.0.unwrap()).1);
                    self.push_code(&displacement.to_le_bytes());
                } else {
                    self.modrm(0b00, regop, RM_SIB);
                    self.sib(0b101, 0b100, 0);
                    self.push_code(&displacement.to_le_bytes());
                }
            } else {
                self.modrm(0b00, regop, reg_encoding(mem.0.unwrap()).1);
            }
        } else {
            if has_displacement {
                if byte_displacement {
                    self.modrm(0b01, regop, RM_SIB);
                } else {
                    self.modrm(0b10, regop, RM_SIB);
                }
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

                if byte_displacement {
                    self.push_code(&(displacement as u8).to_le_bytes());
                } else {
                    self.push_code(&displacement.to_le_bytes());
                }
            }
        }
    }

    pub fn label(&mut self, name: &str) {
        assert!(self.labels.insert(name.to_string(), self.bytes.len()).is_none(),
            "Label {} was already allocated.", name);
    }

    pub fn relocate(&mut self) {
        for (target, offset, size) in &self.relocations {
            use std::convert::TryInto;

            let target = *self.labels.get(target).expect("Label used but not created");
            let rel32: i32 = (target.wrapping_sub(*offset).wrapping_sub(*size) as i64)
                .try_into().expect("Target to far.");

            let write_offset = *offset + *size - 4;
            self.bytes[write_offset..write_offset + 4].copy_from_slice(&rel32.to_le_bytes());
        }

    }
}
