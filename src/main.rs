mod asm;
use asm::Operand::{Imm, Mem, Reg, Rel, Label};
use asm::Reg::*;
use asm::Assembler;

fn main() {
    let mut asm = Assembler::new();

    asm.operand_size(asm::OperandSize::Bits64);



    asm.mov(&[Reg(R9), Imm(10)]);
    asm.mov(&[Reg(R8), Reg(R15)]);
    asm.mov(&[Mem(Some(R15), Some((Rdx, 4)), 0), Imm(1337)]);

    asm.mov(&[Reg(R9), Mem(Some(R13), Some((Rcx, 4)), 0)]);

    asm.label("SS");
    asm.cmp(&[Reg(R15), Mem(Some(Rcx), None, 0xcc)]);
    asm.mov(&[Reg(R13), Imm(i64::MAX)]);
    asm.cmp(&[Reg(R10), Reg(Rcx)]);
    asm.cmp(&[Mem(Some(Rcx), None, 0xcc), Reg(R12)]);
    asm.cmp(&[Mem(Some(Rcx), None, 0xcc), Imm(0x1337)]);

    asm.shl(&[Mem(Some(Rcx), None, 0), Imm(100)]);
    asm.shr(&[Reg(R8), Reg(Rcx)]);

    asm.jmp(&[Rel(0x10)]);
    //asm.pop(&[Mem(Some(Rax), None, 0)]);
    asm.jz(&[Label("SS")]);

    asm.cmovae(&[Reg(Rax), Reg(Rdx)]);

    asm.lea(&[Reg(R9), Mem(Some(R15), Some((Rdx, 2)), 8)]);

    asm.ret(&[]);
    asm.int3(&[]);

    asm.imul(&[Reg(Rcx), Mem(Some(R12), None, 0xcc)]);

    asm.dec(&[Reg(Rdx)]);

    asm.relocate();

    disasm(&asm);
}

fn disasm(asm: &Assembler) {
    std::fs::write("assembly.bin", asm.bytes())
        .expect("Failed to write assembly.bin");

    std::process::Command::new("ndisasm")
        .args(&["-b", "64", "assembly.bin"])
        .spawn()
        .unwrap()
        .wait()
        .unwrap();
}
