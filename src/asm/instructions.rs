use super::{Encoding, Opcode, OpcodeDigit, Assembler, RexMode, Operand};

const DEFAULT_ENCODING: Encoding = Encoding {
    rex:        RexMode::Usable,
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

make_instruction! {
    jmp,
    Encoding {
        rex:   RexMode::Unneeded,
        rel32: Some(Opcode { op: &[0xe9] }),
        reg:   Some(OpcodeDigit { op: &[0xff], digit: 4 }),
        mem:   Some(OpcodeDigit { op: &[0xff], digit: 4 }),
        ..DEFAULT_ENCODING
    }
}

make_instruction! {
    call,
    Encoding {
        rex:   RexMode::Unneeded,
        rel32: Some(Opcode { op: &[0xe8] }),
        reg:   Some(OpcodeDigit { op: &[0xff], digit: 2 }),
        mem:   Some(OpcodeDigit { op: &[0xff], digit: 2 }),
        ..DEFAULT_ENCODING
    }
}

make_instruction! {
    int3,
    Encoding {
        rex:        RexMode::Unneeded,
        standalone: Some(Opcode { op: &[0xcc] }),
        ..DEFAULT_ENCODING
    }
}

make_instruction! {
    mov,
    Encoding {
        rex:      RexMode::Usable,
        regreg:   Some(Opcode { op: &[0x8b] }),
        regmem:   Some(Opcode { op: &[0x8b]}),
        memreg:   Some(Opcode { op: &[0x89]}),
        regimm32: Some(OpcodeDigit { op: &[0xc7], digit: 0 }),
        memimm32: Some(OpcodeDigit { op: &[0xc7], digit: 0 }),
        ..DEFAULT_ENCODING
    }
}

macro_rules! simple_instruction {
    ($name: ident, $reg_regmem: expr, $memreg: expr, $regmem_imm32: expr, $reg: expr) => {
        make_instruction! {
            $name,
            Encoding {
                rex:      RexMode::Usable,
                regreg:   Some(Opcode { op: &[$reg_regmem] }),
                regmem:   Some(Opcode { op: &[$reg_regmem]}),
                memreg:   Some(Opcode { op: &[$memreg]}),
                regimm32: Some(OpcodeDigit { op: &[$regmem_imm32], digit: $reg }),
                memimm32: Some(OpcodeDigit { op: &[$regmem_imm32], digit: $reg }),
                ..DEFAULT_ENCODING
            }
        }
    }
}

make_instruction! {
    push,
    Encoding {
        rex:   RexMode::Implicit,
        reg:   Some(OpcodeDigit { op: &[0xff], digit: 6 }),
        mem:   Some(OpcodeDigit { op: &[0xff], digit: 6 }),
        imm32: Some(Opcode { op: &[0x68] }),
        ..DEFAULT_ENCODING
    }
}

make_instruction! {
    pop,
    Encoding {
        rex:   RexMode::Implicit,
        reg:   Some(OpcodeDigit { op: &[0x8f], digit: 0 }),
        mem:   Some(OpcodeDigit { op: &[0x8f], digit: 0 }),
        ..DEFAULT_ENCODING
    }
}

make_instruction! {
    ret,
    Encoding {
        rex:        RexMode::Unneeded,
        uimm16:     Some(Opcode { op: &[0xc2] }),
        standalone: Some(Opcode { op: &[0xc3] }),
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
                rex:      RexMode::Usable,
                regcl:    Some(OpcodeDigit { op: &[$regmem_cl.0], digit: $regmem_cl.1 }),
                memcl:    Some(OpcodeDigit { op: &[$regmem_cl.0], digit: $regmem_cl.1 }),
                reguimm8: Some(OpcodeDigit { op: &[$regmem_imm8.0], digit: $regmem_imm8.1 }),
                memuimm8: Some(OpcodeDigit { op: &[$regmem_imm8.0], digit: $regmem_imm8.1 }),
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
                rex:   RexMode::Unneeded,
                rel32: Some(Opcode { op: &[0x0f, $code] }),
                ..DEFAULT_ENCODING
            }
        }

        make_instruction! {
            $condmove_name,
            Encoding {
                rex:    RexMode::Usable,
                regmem: Some(Opcode { op: &[0x0f, $code - 0x40] }),
                regreg: Some(Opcode { op: &[0x0f, $code - 0x40] }),
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
        rex: RexMode::Usable,
        reg: Some(OpcodeDigit { op: &[0xf7], digit: 4 }),
        mem: Some(OpcodeDigit { op: &[0xf7], digit: 4 }),
        ..DEFAULT_ENCODING
    }
}

make_instruction! {
    imul,
    Encoding {
        rex:    RexMode::Usable,
        reg:    Some(OpcodeDigit { op: &[0xf7], digit: 5 }),
        mem:    Some(OpcodeDigit { op: &[0xf7], digit: 5 }),
        regmem: Some(Opcode { op: &[0x0f, 0xaf] }),
        ..DEFAULT_ENCODING
    }
}

make_instruction! {
    idiv,
    Encoding {
        rex: RexMode::Usable,
        reg: Some(OpcodeDigit { op: &[0xf7], digit: 7 }),
        mem: Some(OpcodeDigit { op: &[0xf7], digit: 7 }),
        ..DEFAULT_ENCODING
    }
}

make_instruction! {
    div,
    Encoding {
        rex: RexMode::Usable,
        reg: Some(OpcodeDigit { op: &[0xf7], digit: 6 }),
        mem: Some(OpcodeDigit { op: &[0xf7], digit: 6 }),
        ..DEFAULT_ENCODING
    }
}

make_instruction! {
    dec,
    Encoding {
        rex: RexMode::Usable,
        reg: Some(OpcodeDigit { op: &[0xff], digit: 1 }),
        mem: Some(OpcodeDigit { op: &[0xff], digit: 1 }),
        ..DEFAULT_ENCODING
    }
}

make_instruction! {
    inc,
    Encoding {
        rex: RexMode::Usable,
        reg: Some(OpcodeDigit { op: &[0xff], digit: 0 }),
        mem: Some(OpcodeDigit { op: &[0xff], digit: 0 }),
        ..DEFAULT_ENCODING
    }
}
