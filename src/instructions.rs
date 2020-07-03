use super::{Encoding, Opcode, OpcodeDigit, OpcodeRegadd, Assembler, RexMode, Operand, Prefix66Mode};

const DEFAULT_ENCODING: Encoding = Encoding {
    rex:        RexMode::Usable,
    p66:        Prefix66Mode::Usable,
    regreg:     None,
    regreg_inv: None,
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
    regimm64:   None,
    standalone: None,
};

macro_rules! make_instruction {
    ($name: ident, $encoding: expr) => {
        impl Assembler {
            #[inline]
            pub fn $name(&mut self, operands: &[Operand]) {
                const ENCODING: Encoding = $encoding;
                const NAME: &str = stringify!($name);

                self.encode_instruction(operands, &ENCODING, NAME);
            }
        }
    }
}

macro_rules! simple_instruction {
    ($name: ident, $reg_regmem: expr, $memreg: expr, $regmem_imm32: expr, $digit: expr) => {
        make_instruction! {
            $name,
            Encoding {
                rex:      RexMode::Usable,
                p66:      Prefix66Mode::Usable,
                regreg:   Some(Opcode { op: &[$reg_regmem] }),
                regmem:   Some(Opcode { op: &[$reg_regmem] }),
                memreg:   Some(Opcode { op: &[$memreg]}),
                regimm32: Some(OpcodeDigit { op: &[$regmem_imm32], digit: $digit }),
                memimm32: Some(OpcodeDigit { op: &[$regmem_imm32], digit: $digit }),
                ..DEFAULT_ENCODING
            }
        }
    }
}

simple_instruction!(add,  0x03, 0x01, 0x81, 0);
simple_instruction!(sub,  0x2b, 0x29, 0x81, 5);
simple_instruction!(xor,  0x33, 0x31, 0x81, 6);
simple_instruction!(and,  0x23, 0x21, 0x81, 4);
simple_instruction!(or,   0x0b, 0x09, 0x81, 1);
simple_instruction!(cmp,  0x3b, 0x39, 0x81, 7);

macro_rules! shift_instruction {
    ($name: ident, $digit: expr) => {
        make_instruction! {
            $name,
            Encoding {
                rex:      RexMode::Usable,
                p66:      Prefix66Mode::Usable,
                regcl:    Some(OpcodeDigit { op: &[0xd3], digit: $digit }),
                memcl:    Some(OpcodeDigit { op: &[0xd3], digit: $digit }),
                reguimm8: Some(OpcodeDigit { op: &[0xc1], digit: $digit }),
                memuimm8: Some(OpcodeDigit { op: &[0xc1], digit: $digit }),
                ..DEFAULT_ENCODING
            }
        }
    }
}

shift_instruction!(shl, 4);
shift_instruction!(shr, 5);
shift_instruction!(sar, 7);
shift_instruction!(rol, 0);
shift_instruction!(ror, 1);

macro_rules! conditional_instruction {
    ($jcc_name: ident, $cmov_name: ident, $code: expr) => {
        make_instruction! {
            $jcc_name,
            Encoding {
                rex:   RexMode::Unneeded,
                p66:   Prefix66Mode::Unneeded,
                rel32: Some(Opcode { op: &[0x0f, $code] }),
                ..DEFAULT_ENCODING
            }
        }

        make_instruction! {
            $cmov_name,
            Encoding {
                rex:    RexMode::Usable,
                p66:    Prefix66Mode::Usable,
                regmem: Some(Opcode { op: &[0x0f, $code - 0x40] }),
                regreg: Some(Opcode { op: &[0x0f, $code - 0x40] }),
                ..DEFAULT_ENCODING
            }
        }
    }
}

conditional_instruction!(ja,   cmova,   0x87);
conditional_instruction!(jae,  cmovae,  0x83);
conditional_instruction!(jb,   cmovb,   0x82);
conditional_instruction!(jbe,  cmovbe,  0x86);
conditional_instruction!(jc,   cmovc,   0x82);
conditional_instruction!(je,   cmove,   0x84);
conditional_instruction!(jz,   cmovz,   0x84);
conditional_instruction!(jg,   cmovg,   0x8f);
conditional_instruction!(jge,  cmovge,  0x8d);
conditional_instruction!(jl,   cmovl,   0x8c);
conditional_instruction!(jle,  cmovle,  0x8e);
conditional_instruction!(jna,  cmovna,  0x86);
conditional_instruction!(jnae, cmovnae, 0x82);
conditional_instruction!(jnb,  cmovnb,  0x83);
conditional_instruction!(jnbe, cmovnbe, 0x87);
conditional_instruction!(jnc,  cmovnc,  0x83);
conditional_instruction!(jne,  cmovne,  0x85);
conditional_instruction!(jng,  cmovng,  0x8e);
conditional_instruction!(jnge, cmovnge, 0x8c);
conditional_instruction!(jnl,  cmovnl,  0x8d);
conditional_instruction!(jnle, cmovnle, 0x8f);
conditional_instruction!(jno,  cmovno,  0x81);
conditional_instruction!(jnp,  cmovnp,  0x8b);
conditional_instruction!(jns,  cmovns,  0x89);
conditional_instruction!(jnz,  cmovnz,  0x85);
conditional_instruction!(jo,   cmovo,   0x80);
conditional_instruction!(jp,   cmovp,   0x8a);
conditional_instruction!(jpe,  cmovpe,  0x8a);
conditional_instruction!(jpo,  cmovpo,  0x8b);
conditional_instruction!(js,   cmovs,   0x88);

macro_rules! bit_instruction {
    ($name: ident, $regmem_reg_opcode: expr, $regmem_imm_digit: expr) => {
        make_instruction! {
            $name,
            Encoding {
                rex:        RexMode::Usable,
                p66:        Prefix66Mode::Usable,
                reguimm8:   Some(OpcodeDigit { op: &[0x0f, 0xba], digit: $regmem_imm_digit }),
                memuimm8:   Some(OpcodeDigit { op: &[0x0f, 0xba], digit: $regmem_imm_digit }),
                memreg:     Some(Opcode { op: &[0x0f, $regmem_reg_opcode] }),
                regreg_inv: Some(Opcode { op: &[0x0f, $regmem_reg_opcode] }),
                ..DEFAULT_ENCODING
            }
        }
    }
}

bit_instruction!(bt,  0xa3, 4);
bit_instruction!(btc, 0xbb, 7);
bit_instruction!(btr, 0xb3, 6);
bit_instruction!(bts, 0xab, 5);

make_instruction! {
    mov,
    Encoding {
        rex:      RexMode::Usable,
        p66:      Prefix66Mode::Usable,
        regreg:   Some(Opcode { op: &[0x8b] }),
        regmem:   Some(Opcode { op: &[0x8b] }),
        memreg:   Some(Opcode { op: &[0x89] }),
        regimm32: Some(OpcodeDigit  { op: &[0xc7], digit: 0 }),
        memimm32: Some(OpcodeDigit  { op: &[0xc7], digit: 0 }),
        regimm64: Some(OpcodeRegadd { op: &[0xb8] }),
        ..DEFAULT_ENCODING
    }
}

make_instruction! {
    test,
    Encoding {
        rex:        RexMode::Usable,
        p66:        Prefix66Mode::Usable,
        regreg_inv: Some(Opcode { op: &[0x85] }),
        memreg:     Some(Opcode { op: &[0x85] }),
        regimm32:   Some(OpcodeDigit { op: &[0xf7], digit: 0 }),
        memimm32:   Some(OpcodeDigit { op: &[0xf7], digit: 0 }),
        ..DEFAULT_ENCODING
    }
}

make_instruction! {
    push,
    Encoding {
        rex:   RexMode::Implicit,
        p66:   Prefix66Mode::Usable,
        reg:   Some(OpcodeDigit { op: &[0xff], digit: 6 }),
        mem:   Some(OpcodeDigit { op: &[0xff], digit: 6 }),
        imm32: Some(Opcode { op: &[0x68] }),
        ..DEFAULT_ENCODING
    }
}

make_instruction! {
    pop,
    Encoding {
        rex: RexMode::Implicit,
        p66: Prefix66Mode::Usable,
        reg: Some(OpcodeDigit { op: &[0x8f], digit: 0 }),
        mem: Some(OpcodeDigit { op: &[0x8f], digit: 0 }),
        ..DEFAULT_ENCODING
    }
}

make_instruction! {
    ret,
    Encoding {
        rex:        RexMode::Unneeded,
        p66:        Prefix66Mode::Unneeded,
        uimm16:     Some(Opcode { op: &[0xc2] }),
        standalone: Some(Opcode { op: &[0xc3] }),
        ..DEFAULT_ENCODING
    }
}

make_instruction! {
    call,
    Encoding {
        rex:   RexMode::Unneeded,
        p66:   Prefix66Mode::Unneeded,
        rel32: Some(Opcode { op: &[0xe8] }),
        reg:   Some(OpcodeDigit { op: &[0xff], digit: 2 }),
        mem:   Some(OpcodeDigit { op: &[0xff], digit: 2 }),
        ..DEFAULT_ENCODING
    }
}

make_instruction! {
    jmp,
    Encoding {
        rex:   RexMode::Unneeded,
        p66:   Prefix66Mode::Unneeded,
        rel32: Some(Opcode { op: &[0xe9] }),
        reg:   Some(OpcodeDigit { op: &[0xff], digit: 4 }),
        mem:   Some(OpcodeDigit { op: &[0xff], digit: 4 }),
        ..DEFAULT_ENCODING
    }
}

make_instruction! {
    mul,
    Encoding {
        rex: RexMode::Usable,
        p66: Prefix66Mode::Usable,
        reg: Some(OpcodeDigit { op: &[0xf7], digit: 4 }),
        mem: Some(OpcodeDigit { op: &[0xf7], digit: 4 }),
        ..DEFAULT_ENCODING
    }
}

make_instruction! {
    imul,
    Encoding {
        rex:    RexMode::Usable,
        p66:    Prefix66Mode::Usable,
        reg:    Some(OpcodeDigit { op: &[0xf7], digit: 5 }),
        mem:    Some(OpcodeDigit { op: &[0xf7], digit: 5 }),
        regmem: Some(Opcode { op: &[0x0f, 0xaf] }),
        regreg: Some(Opcode { op: &[0x0f, 0xaf] }),
        ..DEFAULT_ENCODING
    }
}

make_instruction! {
    div,
    Encoding {
        rex: RexMode::Usable,
        p66: Prefix66Mode::Usable,
        reg: Some(OpcodeDigit { op: &[0xf7], digit: 6 }),
        mem: Some(OpcodeDigit { op: &[0xf7], digit: 6 }),
        ..DEFAULT_ENCODING
    }
}

make_instruction! {
    idiv,
    Encoding {
        rex: RexMode::Usable,
        p66: Prefix66Mode::Usable,
        reg: Some(OpcodeDigit { op: &[0xf7], digit: 7 }),
        mem: Some(OpcodeDigit { op: &[0xf7], digit: 7 }),
        ..DEFAULT_ENCODING
    }
}

make_instruction! {
    inc,
    Encoding {
        rex: RexMode::Usable,
        p66: Prefix66Mode::Usable,
        reg: Some(OpcodeDigit { op: &[0xff], digit: 0 }),
        mem: Some(OpcodeDigit { op: &[0xff], digit: 0 }),
        ..DEFAULT_ENCODING
    }
}

make_instruction! {
    dec,
    Encoding {
        rex: RexMode::Usable,
        p66: Prefix66Mode::Usable,
        reg: Some(OpcodeDigit { op: &[0xff], digit: 1 }),
        mem: Some(OpcodeDigit { op: &[0xff], digit: 1 }),
        ..DEFAULT_ENCODING
    }
}

make_instruction! {
    neg,
    Encoding {
        rex: RexMode::Usable,
        p66: Prefix66Mode::Usable,
        reg: Some(OpcodeDigit { op: &[0xf7], digit: 3 }),
        mem: Some(OpcodeDigit { op: &[0xf7], digit: 3 }),
        ..DEFAULT_ENCODING
    }
}

make_instruction! {
    not,
    Encoding {
        rex: RexMode::Usable,
        p66: Prefix66Mode::Usable,
        reg: Some(OpcodeDigit { op: &[0xf7], digit: 2 }),
        mem: Some(OpcodeDigit { op: &[0xf7], digit: 2 }),
        ..DEFAULT_ENCODING
    }
}

make_instruction! {
    lea,
    Encoding {
        rex:    RexMode::Usable,
        p66:    Prefix66Mode::Usable,
        regmem: Some(Opcode { op: &[0x8d] }),
        ..DEFAULT_ENCODING
    }
}

make_instruction! {
    movzx,
    Encoding {
        rex:    RexMode::Usable,
        p66:    Prefix66Mode::Unusable,
        regreg: Some(Opcode { op: &[0x0f, 0xb7] }),
        regmem: Some(Opcode { op: &[0x0f, 0xb7] }),
        ..DEFAULT_ENCODING
    }
}

make_instruction! {
    movsx,
    Encoding {
        rex:    RexMode::Usable,
        p66:    Prefix66Mode::Unusable,
        regreg: Some(Opcode { op: &[0x0f, 0xbf] }),
        regmem: Some(Opcode { op: &[0x0f, 0xbf] }),
        ..DEFAULT_ENCODING
    }
}

make_instruction! {
    movsxd,
    Encoding {
        rex:    RexMode::ExplicitRequired,
        p66:    Prefix66Mode::Unusable,
        regreg: Some(Opcode { op: &[0x63] }),
        regmem: Some(Opcode { op: &[0x63] }),
        ..DEFAULT_ENCODING
    }
}

make_instruction! {
    cqo,
    Encoding {
        rex:        RexMode::Usable,
        p66:        Prefix66Mode::Usable,
        standalone: Some(Opcode { op: &[0x99] }),
        ..DEFAULT_ENCODING
    }
}

make_instruction! {
    int3,
    Encoding {
        rex:        RexMode::Unneeded,
        p66:        Prefix66Mode::Unneeded,
        standalone: Some(Opcode { op: &[0xcc] }),
        ..DEFAULT_ENCODING
    }
}

make_instruction! {
    rdtsc,
    Encoding {
        rex:        RexMode::Unneeded,
        p66:        Prefix66Mode::Unneeded,
        standalone: Some(Opcode { op: &[0x0f, 0x31] }),
        ..DEFAULT_ENCODING
    }
}

make_instruction! {
    nop,
    Encoding {
        rex:        RexMode::Unneeded,
        p66:        Prefix66Mode::Unneeded,
        standalone: Some(Opcode { op: &[0x90] }),
        ..DEFAULT_ENCODING
    }
}
