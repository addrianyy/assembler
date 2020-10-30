use super::{Assembler, Operand, Encoding, Opcode, OpcodeDigit, OpcodeRegadd,
            RexwMode, Prefix66Mode};

const DEFAULT_ENCODING: Encoding = Encoding {
    rexw:       RexwMode::Usable,
    p66:        Prefix66Mode::Usable,
    fix_8bit:   false,
    regreg:     None,
    regreg_inv: None,
    regimm32:   None,
    memimm32:   None,
    regimm8:    None,
    memimm8:    None,
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

const DEFAULT_ENCODING_8BIT: Encoding = Encoding {
    rexw:       RexwMode::Unneeded,
    p66:        Prefix66Mode::Unusable,
    fix_8bit:   true,
    regreg:     None,
    regreg_inv: None,
    regimm32:   None,
    memimm32:   None,
    regimm8:    None,
    memimm8:    None,
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
    ($name: ident, $encoding: expr, same_encoding) => {
        make_instruction! { $name, $encoding, Some(Encoding {
            rexw:     DEFAULT_ENCODING_8BIT.rexw,
            p66:      DEFAULT_ENCODING_8BIT.p66,
            fix_8bit: DEFAULT_ENCODING_8BIT.fix_8bit,
            ..$encoding
        }) }
    };
    ($name: ident, $encoding: expr, $encoding_8bit: expr) => {
        impl Assembler {
            #[inline]
            pub fn $name(&mut self, operands: &[Operand]) {
                const NAME: &str = stringify!($name);

                const ENCODING:      Encoding         = $encoding;
                const ENCODING_8BIT: Option<Encoding> = $encoding_8bit;

                self.encode_instruction(operands, &ENCODING, ENCODING_8BIT.as_ref(), NAME);
            }
        }
    };
    ($name: ident, $encoding: expr) => {
        make_instruction! { $name, $encoding, None }
    };
}

macro_rules! simple_instruction {
    ($name: ident, $reg_regmem: expr, $memreg: expr, $regmem_imm32: expr, $digit: expr) => {
        make_instruction! {
            $name,
            Encoding {
                rexw:     RexwMode::Usable,
                p66:      Prefix66Mode::Usable,
                regreg:   Some(Opcode { op: &[$reg_regmem] }),
                regmem:   Some(Opcode { op: &[$reg_regmem] }),
                memreg:   Some(Opcode { op: &[$memreg]}),
                regimm8:  Some(OpcodeDigit { op: &[$regmem_imm32 + 2], digit: $digit }),
                memimm8:  Some(OpcodeDigit { op: &[$regmem_imm32 + 2], digit: $digit }),
                regimm32: Some(OpcodeDigit { op: &[$regmem_imm32], digit: $digit }),
                memimm32: Some(OpcodeDigit { op: &[$regmem_imm32], digit: $digit }),
                ..DEFAULT_ENCODING
            },
            Some(Encoding {
                regreg:   Some(Opcode { op: &[$reg_regmem - 1] }),
                regmem:   Some(Opcode { op: &[$reg_regmem - 1] }),
                memreg:   Some(Opcode { op: &[$memreg - 1]}),
                regimm32: Some(OpcodeDigit { op: &[$regmem_imm32 - 1], digit: $digit }),
                memimm32: Some(OpcodeDigit { op: &[$regmem_imm32 - 1], digit: $digit }),
                ..DEFAULT_ENCODING_8BIT
            })
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
                rexw:     RexwMode::Usable,
                p66:      Prefix66Mode::Usable,
                regcl:    Some(OpcodeDigit { op: &[0xd3], digit: $digit }),
                memcl:    Some(OpcodeDigit { op: &[0xd3], digit: $digit }),
                reguimm8: Some(OpcodeDigit { op: &[0xc1], digit: $digit }),
                memuimm8: Some(OpcodeDigit { op: &[0xc1], digit: $digit }),
                ..DEFAULT_ENCODING
            },
            Some(Encoding {
                regcl:    Some(OpcodeDigit { op: &[0xd2], digit: $digit }),
                memcl:    Some(OpcodeDigit { op: &[0xd2], digit: $digit }),
                reguimm8: Some(OpcodeDigit { op: &[0xc0], digit: $digit }),
                memuimm8: Some(OpcodeDigit { op: &[0xc0], digit: $digit }),
                ..DEFAULT_ENCODING_8BIT
            })
        }
    }
}

shift_instruction!(shl, 4);
shift_instruction!(shr, 5);
shift_instruction!(sar, 7);
shift_instruction!(rol, 0);
shift_instruction!(ror, 1);

macro_rules! conditional_instruction {
    ($jcc_name: ident, $cmov_name: ident, $setcc_name: ident, $code: expr) => {
        make_instruction! {
            $jcc_name,
            Encoding {
                rexw:  RexwMode::Unneeded,
                p66:   Prefix66Mode::Unneeded,
                rel32: Some(Opcode { op: &[0x0f, $code] }),
                ..DEFAULT_ENCODING
            }
        }

        make_instruction! {
            $cmov_name,
            Encoding {
                rexw:   RexwMode::Usable,
                p66:    Prefix66Mode::Usable,
                regmem: Some(Opcode { op: &[0x0f, $code - 0x40] }),
                regreg: Some(Opcode { op: &[0x0f, $code - 0x40] }),
                ..DEFAULT_ENCODING
            }
        }

        make_instruction! {
            $setcc_name,
            Encoding {
                rexw:     RexwMode::Unneeded,
                p66:      Prefix66Mode::Unneeded,
                fix_8bit: true,
                reg:      Some(OpcodeDigit { op: &[0x0f, $code + 0x10], digit: 0 }),
                mem:      Some(OpcodeDigit { op: &[0x0f, $code + 0x10], digit: 0 }),
                ..DEFAULT_ENCODING
            },
            same_encoding
        }
    }
}

conditional_instruction!(ja,   cmova,   seta,   0x87);
conditional_instruction!(jae,  cmovae,  setae,  0x83);
conditional_instruction!(jb,   cmovb,   setb,   0x82);
conditional_instruction!(jbe,  cmovbe,  setbe,  0x86);
conditional_instruction!(jc,   cmovc,   setc,   0x82);
conditional_instruction!(je,   cmove,   sete,   0x84);
conditional_instruction!(jz,   cmovz,   setz,   0x84);
conditional_instruction!(jg,   cmovg,   setg,   0x8f);
conditional_instruction!(jge,  cmovge,  setge,  0x8d);
conditional_instruction!(jl,   cmovl,   setl,   0x8c);
conditional_instruction!(jle,  cmovle,  setle,  0x8e);
conditional_instruction!(jna,  cmovna,  setna,  0x86);
conditional_instruction!(jnae, cmovnae, setnae, 0x82);
conditional_instruction!(jnb,  cmovnb,  setnb,  0x83);
conditional_instruction!(jnbe, cmovnbe, setnbe, 0x87);
conditional_instruction!(jnc,  cmovnc,  setnc,  0x83);
conditional_instruction!(jne,  cmovne,  setne,  0x85);
conditional_instruction!(jng,  cmovng,  setng,  0x8e);
conditional_instruction!(jnge, cmovnge, setnge, 0x8c);
conditional_instruction!(jnl,  cmovnl,  setnl,  0x8d);
conditional_instruction!(jnle, cmovnle, setnle, 0x8f);
conditional_instruction!(jno,  cmovno,  setno,  0x81);
conditional_instruction!(jnp,  cmovnp,  setnp,  0x8b);
conditional_instruction!(jns,  cmovns,  setns,  0x89);
conditional_instruction!(jnz,  cmovnz,  setnz,  0x85);
conditional_instruction!(jo,   cmovo,   seto,   0x80);
conditional_instruction!(jp,   cmovp,   setp,   0x8a);
conditional_instruction!(jpe,  cmovpe,  setpe,  0x8a);
conditional_instruction!(jpo,  cmovpo,  setpo,  0x8b);
conditional_instruction!(js,   cmovs,   sets,   0x88);

macro_rules! bit_instruction {
    ($name: ident, $regmem_reg_opcode: expr, $regmem_imm_digit: expr) => {
        make_instruction! {
            $name,
            Encoding {
                rexw:       RexwMode::Usable,
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

macro_rules! arith_single_instruction {
    ($name: ident, $opcode: expr, $digit: expr) => {
        make_instruction! {
            $name,
            Encoding {
                rexw: RexwMode::Usable,
                p66:  Prefix66Mode::Usable,
                reg:  Some(OpcodeDigit { op: &[$opcode], digit: $digit }),
                mem:  Some(OpcodeDigit { op: &[$opcode], digit: $digit }),
                ..DEFAULT_ENCODING
            },
            Some(Encoding {
                reg: Some(OpcodeDigit { op: &[$opcode - 1], digit: $digit }),
                mem: Some(OpcodeDigit { op: &[$opcode - 1], digit: $digit }),
                ..DEFAULT_ENCODING_8BIT
            })
        }
    }
}

arith_single_instruction!(inc,  0xff, 0);
arith_single_instruction!(dec,  0xff, 1);
arith_single_instruction!(not,  0xf7, 2);
arith_single_instruction!(neg,  0xf7, 3);
arith_single_instruction!(mul,  0xf7, 4);
arith_single_instruction!(div,  0xf7, 6);
arith_single_instruction!(idiv, 0xf7, 7);
// imul is special and allows 2-operand and 3-operand encoding.

macro_rules! standalone_instruction {
    ($name: ident, $prefixes_mode: ident, $opcode: expr, allow_8bit) => {
        make_instruction! {
            $name,
            Encoding {
                rexw:       RexwMode::$prefixes_mode,
                p66:        Prefix66Mode::$prefixes_mode,
                standalone: Some(Opcode { op: $opcode }),
                ..DEFAULT_ENCODING
            },
            same_encoding
        }
    };
    ($name: ident, $prefixes_mode: ident, $opcode: expr) => {
        make_instruction! {
            $name,
            Encoding {
                rexw:       RexwMode::$prefixes_mode,
                p66:        Prefix66Mode::$prefixes_mode,
                standalone: Some(Opcode { op: $opcode }),
                ..DEFAULT_ENCODING
            }
        }
    };
}

standalone_instruction!(cqo,   Usable,   &[0x99]);
standalone_instruction!(int3,  Unneeded, &[0xcc],       allow_8bit);
standalone_instruction!(rdtsc, Unneeded, &[0x0f, 0x31], allow_8bit);
standalone_instruction!(nop,   Unneeded, &[0x90],       allow_8bit);

macro_rules! extension_instruction {
    ($name: ident, $rexw:expr, $p66: expr, $opcode: expr) => {
        make_instruction! {
            $name,
            Encoding {
                rexw:   $rexw,
                p66:    $p66,
                regreg: Some(Opcode { op: $opcode }),
                regmem: Some(Opcode { op: $opcode }),
                ..DEFAULT_ENCODING
            }
        }
    }
}
extension_instruction!(movzxb, RexwMode::Usable,           Prefix66Mode::Usable,   &[0x0f, 0xb6]);
extension_instruction!(movzxw, RexwMode::Usable,           Prefix66Mode::Unusable, &[0x0f, 0xb7]);
extension_instruction!(movsxb, RexwMode::Usable,           Prefix66Mode::Usable,   &[0x0f, 0xbe]);
extension_instruction!(movsxw, RexwMode::Usable,           Prefix66Mode::Unusable, &[0x0f, 0xbf]);
extension_instruction!(movsxd, RexwMode::ExplicitRequired, Prefix66Mode::Unusable, &[0x63]);

make_instruction! {
    mov,
    Encoding {
        rexw:     RexwMode::Usable,
        p66:      Prefix66Mode::Usable,
        regreg:   Some(Opcode { op: &[0x8b] }),
        regmem:   Some(Opcode { op: &[0x8b] }),
        memreg:   Some(Opcode { op: &[0x89] }),
        regimm32: Some(OpcodeDigit  { op: &[0xc7], digit: 0 }),
        memimm32: Some(OpcodeDigit  { op: &[0xc7], digit: 0 }),
        regimm64: Some(OpcodeRegadd { op: &[0xb8] }),
        ..DEFAULT_ENCODING
    },
    Some(Encoding {
        regreg:   Some(Opcode { op: &[0x8a] }),
        regmem:   Some(Opcode { op: &[0x8a] }),
        memreg:   Some(Opcode { op: &[0x88] }),
        regimm32: Some(OpcodeDigit  { op: &[0xc6], digit: 0 }),
        memimm32: Some(OpcodeDigit  { op: &[0xc6], digit: 0 }),
        ..DEFAULT_ENCODING_8BIT
    })
}

make_instruction! {
    test,
    Encoding {
        rexw:       RexwMode::Usable,
        p66:        Prefix66Mode::Usable,
        regreg_inv: Some(Opcode { op: &[0x85] }),
        memreg:     Some(Opcode { op: &[0x85] }),
        regimm32:   Some(OpcodeDigit { op: &[0xf7], digit: 0 }),
        memimm32:   Some(OpcodeDigit { op: &[0xf7], digit: 0 }),
        ..DEFAULT_ENCODING
    },
    Some(Encoding {
        regreg_inv: Some(Opcode { op: &[0x84] }),
        memreg:     Some(Opcode { op: &[0x84] }),
        regimm32:   Some(OpcodeDigit { op: &[0xf6], digit: 0 }),
        memimm32:   Some(OpcodeDigit { op: &[0xf6], digit: 0 }),
        ..DEFAULT_ENCODING_8BIT
    })
}

make_instruction! {
    push,
    Encoding {
        rexw:  RexwMode::Implicit,
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
        rexw: RexwMode::Implicit,
        p66:  Prefix66Mode::Usable,
        reg:  Some(OpcodeDigit { op: &[0x8f], digit: 0 }),
        mem:  Some(OpcodeDigit { op: &[0x8f], digit: 0 }),
        ..DEFAULT_ENCODING
    }
}

make_instruction! {
    jmp,
    Encoding {
        rexw:  RexwMode::Unneeded,
        p66:   Prefix66Mode::Unneeded,
        rel32: Some(Opcode { op: &[0xe9] }),
        reg:   Some(OpcodeDigit { op: &[0xff], digit: 4 }),
        mem:   Some(OpcodeDigit { op: &[0xff], digit: 4 }),
        ..DEFAULT_ENCODING
    }
}

make_instruction! {
    call,
    Encoding {
        rexw:  RexwMode::Unneeded,
        p66:   Prefix66Mode::Unneeded,
        rel32: Some(Opcode { op: &[0xe8] }),
        reg:   Some(OpcodeDigit { op: &[0xff], digit: 2 }),
        mem:   Some(OpcodeDigit { op: &[0xff], digit: 2 }),
        ..DEFAULT_ENCODING
    }
}

make_instruction! {
    ret,
    Encoding {
        rexw:       RexwMode::Unneeded,
        p66:        Prefix66Mode::Unneeded,
        uimm16:     Some(Opcode { op: &[0xc2] }),
        standalone: Some(Opcode { op: &[0xc3] }),
        ..DEFAULT_ENCODING
    }
}

make_instruction! {
    imul,
    Encoding {
        rexw:   RexwMode::Usable,
        p66:    Prefix66Mode::Usable,
        reg:    Some(OpcodeDigit { op: &[0xf7], digit: 5 }),
        mem:    Some(OpcodeDigit { op: &[0xf7], digit: 5 }),
        regmem: Some(Opcode { op: &[0x0f, 0xaf] }),
        regreg: Some(Opcode { op: &[0x0f, 0xaf] }),
        ..DEFAULT_ENCODING
    },
    Some(Encoding {
        reg: Some(OpcodeDigit { op: &[0xf6], digit: 5 }),
        mem: Some(OpcodeDigit { op: &[0xf6], digit: 5 }),
        ..DEFAULT_ENCODING_8BIT
    })
}

make_instruction! {
    lea,
    Encoding {
        rexw:   RexwMode::Usable,
        p66:    Prefix66Mode::Usable,
        regmem: Some(Opcode { op: &[0x8d] }),
        ..DEFAULT_ENCODING
    }
}
