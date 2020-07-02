use asm::Operand::{Imm, Mem, Reg, Rel, Label};
use asm::Reg::*;
use asm::Assembler;

fn main() {
    let mut asm = Assembler::new();

    asm.operand_size(asm::OperandSize::Bits16);

    //asm.movzx(&[Reg(Rcx), Reg(Rdx)]);
    //asm.movzx(&[Reg(Rcx), Mem(Some(Rdx), None, 0)]);

    //asm.raw_instruction(&[0x66]);
    asm.mov(&[Reg(R9), Imm(10)]);
    asm.mov(&[Reg(R8), Reg(R15)]);
    asm.mov(&[Mem(Some(R15), Some((Rdx, 4)), 0), Imm(1337)]);

    asm.mov(&[Reg(R9), Mem(Some(R13), Some((Rcx, 4)), 0)]);

    asm.label("SS");
    asm.cmp(&[Reg(R15), Mem(Some(Rcx), None, 0xcc)]);
    //asm.mov(&[Reg(Rsp), Imm(i64::MAX)]);
    asm.cmp(&[Reg(R10), Reg(Rcx)]);
    asm.cmp(&[Mem(Some(Rcx), None, 0xcc), Reg(R12)]);
    asm.cmp(&[Mem(Some(Rcx), None, 0xcc), Imm(0x1337)]);

    asm.shl(&[Mem(Some(Rcx), None, 0), Imm(100)]);
    asm.shr(&[Reg(R8), Reg(Rcx)]);

    //asm.push(&[Reg(Rax)]);

    asm.jmp(&[Rel(0x10)]);
    asm.raw_instruction(b"\xcc");
    //asm.pop(&[Mem(Some(Rax), None, 0)]);
    asm.jz(&[Label("SS")]);

    asm.cmovae(&[Reg(Rax), Reg(Rdx)]);

    asm.ror(&[Reg(R8), Reg(Rcx)]);

    asm.lea(&[Reg(R9), Mem(Some(R15), Some((Rdx, 2)), 8)]);

    asm.ret(&[Imm(16)]);
    asm.int3(&[]);

    asm.imul(&[Reg(Rcx), Mem(Some(R12), None, 0xcc)]);
    asm.imul(&[Reg(Rcx), Reg(R13)]);

    asm.dec(&[Reg(Rdx)]);
    asm.test(&[Reg(Rcx), Reg(Rdx)]);
    asm.lea(&[Reg(Rdx), Mem(Some(Rbx), None, 99)]);
    asm.int3(&[]);

    asm.inc(&[Reg(R11)]);
    //asm.pop(&[Reg(R10)]);

    //asm.operand_size(asm::OperandSize::Bits32);
    //asm.movsx(&[Reg(Rax), Reg(R15)]);
    //asm.movsx(&[Reg(Rcx), Mem(Some(R9), None, 0xcc)]);

    asm.mov(&[Reg(Rbx), Imm(-1337)]);

    //asm.operand_size(asm::OperandSize::Bits64);
    asm.cqo(&[]);

    asm.neg(&[Reg(R14)]);
    asm.not(&[Mem(Some(Rcx), None, 0)]);

    asm.call(&[Mem(Some(Rdx), None, 1)]);

    //asm.movsx(&[Reg(Rax), Reg(Rax)]);
    //asm.movsxd(&[Reg(Rax), Reg(Rax)]);

    asm.mov(&[Mem(Some(Rdx), Some((Rax, 2)), 0x10), Reg(Rcx)]);

    for i in 0..1 {
        asm.mov(&[Reg(Rax), Reg(Rdx)]);
        asm.shr(&[Reg(Rax), Imm(16 + 9 * i)]);
        asm.and(&[Reg(Rax), Imm(0x1ff)]);
        asm.mov(&[Reg(Rcx), Mem(Some(Rcx), Some((Rax, 8)), 0)]);
        asm.test(&[Reg(Rcx), Reg(Rcx)]);
        asm.jz(&[Label("access_fault")]);
    }

    asm.mov(&[Reg(Rax), Reg(Rcx)]);
    asm.ret(&[]);

    asm.label("access_fault");
    asm.int3(&[]);

    disasm(&mut asm);
}

fn disasm(asm: &mut Assembler) {
    std::fs::write("assembly.bin", asm.bytes())
        .expect("Failed to write assembly.bin");

    std::process::Command::new("ndisasm")
        .args(&["-b", "64", "assembly.bin"])
        .spawn()
        .unwrap()
        .wait()
        .unwrap();
}
