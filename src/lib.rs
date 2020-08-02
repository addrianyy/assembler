mod instructions;
use std::collections::HashMap;
use std::borrow::Cow;

const MOD_DIRECT: u8 = 0b11;

#[derive(Copy, Clone, PartialEq, Eq)]
enum RexMode {
    ExplicitRequired,
    SilentRequired,
    Implicit,
    Usable,
    Unneeded,
}

#[derive(Copy, Clone, PartialEq, Eq)]
enum Prefix66Mode {
    Unusable,
    Usable,
    Unneeded,
}

struct Opcode {
    op: &'static [u8],
}

struct OpcodeDigit {
    op:    &'static [u8],
    digit: u8,
}

struct OpcodeRegadd {
    op: &'static [u8],
}

struct Encoding {
    rex:        RexMode,
    p66:        Prefix66Mode,

    /// r64, r/m64
    regreg:     Option<Opcode>,

    /// r/m64, r64
    regreg_inv: Option<Opcode>,

    regimm32:   Option<OpcodeDigit>,
    memimm32:   Option<OpcodeDigit>,
    reguimm8:   Option<OpcodeDigit>,
    memuimm8:   Option<OpcodeDigit>,
    regmem:     Option<Opcode>,
    memreg:     Option<Opcode>,
    regcl:      Option<OpcodeDigit>,
    memcl:      Option<OpcodeDigit>,
    reg:        Option<OpcodeDigit>,
    mem:        Option<OpcodeDigit>,
    rel32:      Option<Opcode>,
    imm32:      Option<Opcode>,
    uimm16:     Option<Opcode>,
    regimm64:   Option<OpcodeRegadd>,
    standalone: Option<Opcode>,
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
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

impl Reg {
    fn encoding(&self) -> (bool, u8) {
        match self {
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
}

type MemOperand = (Option<Reg>, Option<(Reg, usize)>, i64);

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum Operand<'a> {
    Reg(Reg),
    Imm(i64),
    Mem(Option<Reg>, Option<(Reg, usize)>, i64),
    Rel(i32),
    Label(&'a str),
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum OperandSize {
    Bits64 = 64,
    Bits32 = 32,
    Bits16 = 16,
}

#[derive(Copy, Clone, PartialEq, Eq, Hash)]
struct LabelID(usize);

pub struct Assembler {
    bytes:              Vec<u8>,
    operand_size:       OperandSize,

    labels:             HashMap<LabelID, usize>,
    fixups:             Vec<(LabelID, usize, usize)>,
    current_label:      Option<LabelID>,

    label_to_id:        HashMap<String, LabelID>,
    next_free_label_id: LabelID,

}

impl Default for Assembler {
    fn default() -> Self {
        Self::new()
    }
}

impl Assembler {
    pub fn new() -> Self {
        Self::with_operand_size(OperandSize::Bits64)
    }

    pub fn with_operand_size(operand_size: OperandSize) -> Self {
        Self {
            operand_size,
            bytes:              Vec::new(),
            labels:             HashMap::new(),
            fixups:             Vec::new(),
            current_label:      None,
            label_to_id:        HashMap::new(),
            next_free_label_id: LabelID(0),
        }
    }

    pub fn operand_size(&mut self, size: OperandSize) {
        self.operand_size = size;
    }

    pub fn label(&mut self, label: &str) {
        let is_local = label.starts_with('.');
        let label_id = self.label_to_id(label);

        if !is_local {
            self.current_label = Some(label_id);
        }

        assert!(self.labels.insert(label_id, self.current_offset()).is_none(),
            "Label {} was already assigned.", label);
    }

    pub fn into_bytes(mut self) -> Vec<u8> {
        self.apply_fixups();

        self.bytes
    }

    pub fn bytes(&mut self) -> &[u8] {
        self.apply_fixups();
        self.fixups.clear();

        &self.bytes
    }

    pub fn current_offset(&self) -> usize {
        self.bytes.len()
    }

    pub fn raw_instruction(&mut self, instruction: &[u8]) {
        self.push_code(instruction);
    }

    fn label_to_id(&mut self, original_label: &str) -> LabelID {
        let label = if original_label.starts_with('.') {
            let suffix = self.current_label.map(|x| x.0.to_string())
                .unwrap_or_else(|| String::from("entry"));

            Cow::Owned(format!("{}_{}", original_label, suffix))
        } else {
            Cow::Borrowed(original_label)
        };

        match self.label_to_id.get(label.as_ref()) {
            Some(label_id) => *label_id,
            None           => {
                let label_id = self.next_free_label_id;

                assert!(self.label_to_id.insert(label.into_owned(), label_id).is_none(),
                    "Internal error: Label {} was already created.", original_label);

                self.next_free_label_id = LabelID(label_id.0
                    .checked_add(1).expect("Label IDs overflowed"));

                label_id
            }
        }
    }

    fn apply_fixups(&mut self) {
        for &(ref label, offset, size) in &self.fixups {
            use std::convert::TryInto;

            let target = match self.labels.get(label) {
                Some(target) => target,
                None         => {
                    let name = self.label_to_id.iter()
                        .find(|(_, label_id)| *label_id == label)
                        .expect("Internal error: Unnamed label was referenced.").0;

                    panic!("Non-existent label {} was referenced.", name);
                }
            };

            assert!(size > 4, "Internal error: instruction is too small to have rel32.");

            // target = offset + instruction_size + rel32
            // rel32  = target - offset - instruction_size
            let rel32: i32 = (target.wrapping_sub(offset).wrapping_sub(size) as i64)
                .try_into().expect("Cannot fixup, target is too far for rel32.");

            // rel32 is always encoded in last 4 bytes of instruction.
            let write_offset = offset + size - 4;

            self.bytes[write_offset..write_offset + 4]
                .copy_from_slice(&rel32.to_le_bytes());
        }
    }

    fn require_64bit(&self) {
        assert!(self.operand_size == OperandSize::Bits64,
            "This operation cannot be done with {} bit operand size.", self.operand_size as usize);
    }

    fn get_rexw(&self, encoding: &Encoding) -> bool {
        match encoding.rex {
            RexMode::SilentRequired => {
                true
            }
            RexMode::Implicit => {
                // Instructions with implicit REX.W can possibly be encoded with 16 bit operand
                // size (but not 32).
                if self.operand_size == OperandSize::Bits16 &&
                        encoding.p66 == Prefix66Mode::Usable {
                    return false;
                }

                self.require_64bit();
                false
            }
            RexMode::ExplicitRequired => {
                self.require_64bit();
                true
            }
            RexMode::Usable   => self.operand_size == OperandSize::Bits64,
            RexMode::Unneeded => false,
        }
    }

    fn override_operand_size(&mut self, encoding: &Encoding) {
        if self.operand_size == OperandSize::Bits16 {
            match encoding.p66 {
                Prefix66Mode::Unneeded => (),
                Prefix66Mode::Unusable => {
                    panic!("This operation cannot be done with 16 bit operand size.");
                }
                Prefix66Mode::Usable => {
                    self.push_code(&[0x66]);
                }
            }
        }
    }

    fn encode_regreg(&mut self, reg1: Reg, reg2: Reg, op: &Opcode, encoding: &Encoding) {
        let (reg1_e, reg1_enc) = reg1.encoding();
        let (reg2_e, reg2_enc) = reg2.encoding();

        self.override_operand_size(encoding);
        self.rex(self.get_rexw(encoding), reg1_e, false, reg2_e);
        self.push_code(&op.op);
        self.modrm(MOD_DIRECT, reg1_enc, reg2_enc);
    }

    fn encode_regimm(&mut self, reg: Reg, imm: i64, size: usize,
                     op: &OpcodeDigit, encoding: &Encoding) {
        let (reg_e, reg_enc) = reg.encoding();

        self.override_operand_size(encoding);
        self.rex(self.get_rexw(encoding), false, false, reg_e);
        self.push_code(&op.op);
        self.modrm(MOD_DIRECT, op.digit, reg_enc);
        self.push_imm(imm, size);
    }

    fn encode_memreg_regmem(&mut self, reg: Reg, mem: MemOperand,
                            op: &Opcode, encoding: &Encoding) {
        let (reg_e, reg_enc) = reg.encoding();

        self.override_operand_size(encoding);
        self.encode_memory_operand(reg_enc, reg_e, self.get_rexw(encoding), op.op, mem);
    }

    fn encode_memimm(&mut self, mem: MemOperand, imm: i64, size: usize,
                     op: &OpcodeDigit, encoding: &Encoding) {
        self.override_operand_size(encoding);
        self.encode_memory_operand(op.digit, false, self.get_rexw(encoding), op.op, mem);
        self.push_imm(imm, size);
    }

    fn encode_mem(&mut self, mem: MemOperand, op: &OpcodeDigit, encoding: &Encoding) {
        self.override_operand_size(encoding);
        self.encode_memory_operand(op.digit, false, self.get_rexw(encoding), op.op, mem);
    }

    fn encode_reg(&mut self, reg: Reg, op: &OpcodeDigit, encoding: &Encoding) {
        let (reg_e, reg_enc) = reg.encoding();

        self.override_operand_size(encoding);
        self.rex(self.get_rexw(encoding), false, false, reg_e);
        self.push_code(&op.op);
        self.modrm(MOD_DIRECT, op.digit, reg_enc);
    }

    fn encode_imm(&mut self, imm: i64, size: usize, op: &Opcode, encoding: &Encoding) {
        self.override_operand_size(encoding);
        self.rex(self.get_rexw(encoding), false, false, false);
        self.push_code(&op.op);
        self.push_imm(imm, size);
    }

    fn encode_standalone(&mut self, op: &Opcode, encoding: &Encoding) {
        self.override_operand_size(encoding);
        self.rex(self.get_rexw(encoding), false, false, false);
        self.push_code(&op.op);
    }

    fn encode_rel32(&mut self, rel: i32, label: Option<&str>, op: &Opcode, encoding: &Encoding) {
        assert!(encoding.rex == RexMode::Unneeded && encoding.p66 == Prefix66Mode::Unneeded,
            "Relative jumps/calls should not need REX or 66 prefix.");

        let offset_before = self.current_offset();

        self.encode_imm(rel as i64, 4, op, encoding);

        if let Some(label) = label {
            assert!(rel == 0, "Target label was specified but relative offset was not 0.");

            let inst_size = self.current_offset() - offset_before;
            let label_id  = self.label_to_id(label);

            self.fixups.push((label_id, offset_before, inst_size));
        }
    }

    fn encode_regimm64(&mut self, reg: Reg, imm: i64, op: &OpcodeRegadd, _encoding: &Encoding) {
        self.require_64bit();

        assert!(op.op.len() == 1, "Only 1 byte opcodes for r64, imm64 are supported.");

        let (reg_e, reg_enc) = reg.encoding();

        let op = op.op[0] + reg_enc;

        self.rex(true, false, false, reg_e);
        self.push_code(&[op]);
        self.push_imm(imm, 8);
    }

    fn encode_instruction(&mut self, operands: &[Operand], encoding: &Encoding, name: &str) {
        macro_rules! fits_within {
            ($value: expr, $type: tt) => {
                $value as i64 <= $type::MAX as i64 && $value as i64 >= $type::MIN as i64
            }
        }

        // imm32s get truncated to 16 bits with 16 bit operand size.

        macro_rules! fits_within_imm32 {
            ($value: expr) => {
                match self.operand_size {
                    OperandSize::Bits16 => fits_within!($value, i16),
                    _                   => fits_within!($value, i32),
                }
            }
        }

        let imm32_size = match self.operand_size {
            OperandSize::Bits16 => 2,
            _                   => 4,
        };

        match *operands {
            [] if encoding.standalone.is_some() => {
                self.encode_standalone(encoding.standalone.as_ref().unwrap(), encoding);
            }
            [Operand::Rel(rel)]
                if encoding.rel32.is_some() =>
            {
                self.encode_rel32(rel, None, encoding.rel32.as_ref().unwrap(), encoding);
            }
            [Operand::Label(label)]
                if encoding.rel32.is_some() =>
            {
                self.encode_rel32(0, Some(label), encoding.rel32.as_ref().unwrap(), encoding);
            }
            [Operand::Reg(reg)]
                if encoding.reg.is_some() =>
            {
                self.encode_reg(reg, encoding.reg.as_ref().unwrap(), encoding);
            }
            [Operand::Mem(base, index, disp)]
                if encoding.mem.is_some() =>
            {
                self.encode_mem((base, index, disp), encoding.mem.as_ref().unwrap(), encoding);
            }
            [Operand::Imm(imm)]
                if fits_within!(imm, u16) && encoding.uimm16.is_some() =>
            {
                self.encode_imm(imm, 2, encoding.uimm16.as_ref().unwrap(), encoding);
            }
            [Operand::Imm(imm)]
                if fits_within_imm32!(imm) && encoding.imm32.is_some() =>
            {
                self.encode_imm(imm, imm32_size, encoding.imm32.as_ref().unwrap(), encoding);
            }
            [Operand::Reg(reg1), Operand::Reg(reg2)]
                if encoding.regreg.is_some() || encoding.regreg_inv.is_some() =>
            {
                // If instruction encoding is r/m64, r64 we need to invert
                // operand order.
                match (encoding.regreg.as_ref(), encoding.regreg_inv.as_ref()) {
                    (Some(regreg), None) => {
                        self.encode_regreg(reg1, reg2, regreg, encoding);
                    }
                    (None, Some(regreg_inv)) => {
                        self.encode_regreg(reg2, reg1, regreg_inv, encoding);
                    }
                    _ => panic!("Regreg and inverted regreg both implemented for {}.", name),
                }
            }
            [Operand::Reg(reg), Operand::Imm(imm)]
                if fits_within!(imm, u8) && encoding.reguimm8.is_some() =>
            {
                self.encode_regimm(reg, imm, 1, encoding.reguimm8.as_ref().unwrap(), encoding);
            }
            [Operand::Mem(base, index, disp), Operand::Imm(imm)]
                if fits_within!(imm, u8) && encoding.memuimm8.is_some() =>
            {
                self.encode_memimm((base, index, disp), imm, 1,
                    encoding.memuimm8.as_ref().unwrap(), encoding);
            }
            [Operand::Reg(reg), Operand::Imm(imm)]
                if fits_within_imm32!(imm) && encoding.regimm32.is_some() =>
            {
                self.encode_regimm(reg, imm, imm32_size,
                    encoding.regimm32.as_ref().unwrap(), encoding);
            }
            [Operand::Mem(base, index, disp), Operand::Imm(imm)]
                if fits_within_imm32!(imm) && encoding.memimm32.is_some() =>
            {
                self.encode_memimm((base, index, disp),
                    imm, imm32_size, encoding.memimm32.as_ref().unwrap(), encoding);
            }
            [Operand::Reg(reg), Operand::Imm(imm)]
                if encoding.regimm64.is_some() =>
            {
                self.encode_regimm64(reg, imm, encoding.regimm64.as_ref().unwrap(), encoding);
            }
            [Operand::Reg(reg), Operand::Mem(base, index, disp)]
                if encoding.regmem.is_some() =>
            {
                self.encode_memreg_regmem(reg, (base, index, disp),
                    encoding.regmem.as_ref().unwrap(), encoding);
            }
            [Operand::Mem(base, index, disp), Operand::Reg(reg)]
                if encoding.memreg.is_some() =>
            {
                self.encode_memreg_regmem(reg, (base, index, disp),
                    encoding.memreg.as_ref().unwrap(), encoding);
            }
            [Operand::Reg(reg), Operand::Reg(Reg::Rcx)]
                if encoding.regcl.is_some() =>
            {
                self.encode_reg(reg, encoding.regcl.as_ref().unwrap(), encoding);
            }
            [Operand::Mem(base, index, disp), Operand::Reg(Reg::Rcx)]
                if encoding.memcl.is_some() =>
            {
                self.encode_mem((base, index, disp), encoding.memcl.as_ref().unwrap(), encoding);
            }
            _ => {
                panic!("{:?} operand combination is unsupported for instruction {} \
                        with {} bit operand size.", operands, name,
                        self.operand_size as usize);
            }
        }
    }

    fn push_code(&mut self, bytes: &[u8]) {
        self.bytes.extend_from_slice(bytes);
    }

    fn push_imm(&mut self, imm: i64, size: usize) {
        let bytes = &imm.to_le_bytes();

        // Truncated part needs to be either all 0s (positive number) or all fs (negative number).
        let all_0s = bytes[size..].iter().all(|x| *x == 0x00);
        let all_fs = bytes[size..].iter().all(|x| *x == 0xff);

        assert!(all_0s || all_fs,
            "Truncation of imm {} to {} bytes will cause data loss.", imm, size);

        self.push_code(&bytes[..size]);
    }

    fn rex(&mut self, w: bool, r: bool, x: bool, b: bool) {
        // REX without any attributes does not need to be emited because it doesn't change anyting.
        // In 8 bit mode it makes it impossible to encode high registers (AH, CH, ..)
        // but this assembler doesn't support 8 bit operand size anyway so we ignore that.
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

    fn encode_memory_operand(&mut self, regop: u8, rex_r: bool, rex_w: bool, opcode: &[u8],
                             mem: MemOperand) {
        let mut require_sib = false;

        const RM_SIB:  u8 = 0b100;
        const RM_DISP: u8 = 0b101;

        let mut mem = if mem.2 == 0 { (mem.0, mem.1, None) } else { (mem.0, mem.1, Some(mem.2)) };

        // Set if operand contains index register but not base one. It requires special handling.
        let mut index_no_base = false;

        if let Some(base) = mem.0 {
            let encoding = base.encoding().1;

            // SIB is required to encode RSP or R12 as a base register.
            if encoding == RM_SIB {
                require_sib = true;
            }

            // Displacement is required to encode RBP or R13 as a base register.
            if encoding == RM_DISP && mem.2.is_none() {
                mem.2 = Some(0);
            }
        } else {
            // There must be at least one component in memory operand.
            assert!(mem.1.is_some() || mem.2.is_some(), "Memory operand is empty.");

            if mem.1.is_some() {
                index_no_base = true;

                // 32 bit displacement is required to encode memory operand with index register but
                // without base one.
                if mem.2.is_none() {
                    mem.2 = Some(0);
                }

                // Base register RBP will be ignored if MODRM.MOD == 0.
                mem.0 = Some(Reg::Rbp);
            }
        }

        if let Some((index, _)) = mem.1 {
            // Special case: R12 can be used as an index register even though it has
            // the same 3-bit encoding as RSP (which cannot).
            assert!(index != Reg::Rsp, "RSP cannot be used as an index register.");

            require_sib = true;
        }

        let mut byte_displacement = false;

        if let Some(displacement) = mem.2 {
            // Displacement must fit in 32 bit signed integer.
            if !(displacement >= i32::MIN as i64 && displacement <= i32::MAX as i64) {
                panic!("Displacement {} is too big.", displacement);
            }

            // It is possible to use 1 byte displacement if it fits in 8 bit signed integer.
            if displacement >= i8::MIN as i64 && displacement <= i8::MAX as i64 {
                byte_displacement = true;
            }
        }

        // If registers are not present they don't need extension in RAX by default.
        let rex_b = mem.0.map(|reg| reg.encoding().0).unwrap_or(false);
        let rex_x = mem.1.map(|reg| reg.0.encoding().0).unwrap_or(false);

        self.rex(rex_w, rex_r, rex_x, rex_b);
        self.push_code(opcode);

        let has_base         = mem.0.is_some();
        let has_displacement = mem.2.is_some();

        if !require_sib {
            assert!(!index_no_base, "There cannot be index register if SIB is not required.");

            if has_displacement {
                let displacement = mem.2.unwrap() as i32;

                if has_base {
                    let base_encoding = mem.0.unwrap().encoding().1;

                    if byte_displacement {
                        // Encode base register and 8 bit displacement (without SIB).
                        self.modrm(0b01, regop, base_encoding);
                        self.push_code(&(displacement as i8).to_le_bytes());
                    } else {
                        // Encode base register and 32 bit displacement (without SIB).
                        self.modrm(0b10, regop, base_encoding);
                        self.push_code(&displacement.to_le_bytes());
                    }
                } else {
                    // In 64 bit mode the only way to encode displacement-only operand is to use
                    // SIB byte with these special values. Displacement must be 32 bit wide.
                    // Encoding without SIB is RIP-relative.
                    self.modrm(0b00, regop, RM_SIB);
                    self.sib(0b101, 0b100, 0);
                    self.push_code(&displacement.to_le_bytes());
                }
            } else {
                // Simple case: we just need to encode base register.
                self.modrm(0b00, regop, mem.0.unwrap().encoding().1);
            }
        } else {
            if has_displacement && !index_no_base {
                // Pick the shortest possible displacement encoding.
                if byte_displacement {
                    self.modrm(0b01, regop, RM_SIB);
                } else {
                    self.modrm(0b10, regop, RM_SIB);
                }
            } else {
                // No displacement is needed or index_no_base is true. In that case
                // base will be RBP and 32-bit displacement still will be required.
                self.modrm(0b00, regop, RM_SIB);
            }

            // If index_no_base is set then MODRM.MOD == 00 and base register == RBP
            // so base register will be ignored.
            let base = mem.0.unwrap();

            // RSP encoding means no index (scale is ignored then).
            let index = mem.1.unwrap_or((Reg::Rsp, 1));

            // Convert user-friendly scale to x86 one.
            let scale = match index.1 {
                1 => 0,
                2 => 1,
                4 => 2,
                8 => 3,
                _ => panic!("Invalid scale {}.", index.1),
            };

            self.sib(base.encoding().1, index.0.encoding().1, scale);

            if has_displacement {
                let displacement = mem.2.unwrap() as i32;

                // Encode displacement using 8 bit version if possible.
                // If no base register was specified displacement is forced to be 32-bit wide.
                if byte_displacement && !index_no_base {
                    self.push_code(&(displacement as i8).to_le_bytes());
                } else {
                    self.push_code(&displacement.to_le_bytes());
                }
            }
        }
    }
}
