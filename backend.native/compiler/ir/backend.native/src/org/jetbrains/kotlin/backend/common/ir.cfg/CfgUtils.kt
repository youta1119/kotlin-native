package org.jetbrains.kotlin.backend.common.ir.cfg

//-----------------------------------------------------------------------------//

val CfgNull = Constant(TypeUnit, "null")

//--- Operand -----------------------------------------------------------------//

fun Operand.addDef(instruction: Instruction) { defs.add(instruction) }
fun Operand.addUse(instruction: Instruction) { uses.add(instruction) }

//--- Instruction -------------------------------------------------------------//

fun Instruction.addUse(operand: Operand)  { operand.addUse(this); uses.add(operand) }
fun Instruction.addDef(operand: Variable) { operand.addDef(this); defs.add(operand) }

//--- Block -------------------------------------------------------------------//

fun Block.addSuccessor(successor: Block) {
    successors.add(successor)
    successor.predecessors.add(this)
}

//-----------------------------------------------------------------------------//

fun Block.instruction(opcode: Opcode, vararg uses: Operand): Instruction {
    val instruction = Instruction(opcode)
    instructions.add(instruction)
    uses.forEach(instruction::addUse)
    return instruction
}

//-----------------------------------------------------------------------------//

fun Block.instruction(opcode: Opcode, def: Variable, vararg uses: Operand): Instruction {
    val instruction = Instruction(opcode)
    instructions.add(instruction)
    uses.forEach(instruction::addUse)
    instruction.addDef(def)
    return instruction
}

//-----------------------------------------------------------------------------//

fun Block.mov(def: Variable, use: Operand) {

    val instruction = instruction(Opcode.mov)
    instruction.addUse(use)
    instruction.addDef(def)
}

//-----------------------------------------------------------------------------//

fun Block.ret(use: Operand) {
    val instruction = instruction(Opcode.ret)
    instruction.addUse(use)
}

//-----------------------------------------------------------------------------//

fun Block.br(target: Block) {
    val instruction   = instruction(Opcode.br)
    val targetOperand = Constant(TypeBlock, target)
    instruction.addUse(targetOperand)

    addSuccessor(target)
}

//-----------------------------------------------------------------------------//

fun Block.condBr(condition: Operand, targetTrue: Block, targetFalse: Block) {
    val instruction = instruction(Opcode.condbr)
    val targetTrueOperand  = Constant(TypeBlock, targetTrue)
    val targetFalseOperand = Constant(TypeBlock, targetFalse)
    instruction.addUse(condition)
    instruction.addUse(targetTrueOperand)
    instruction.addUse(targetFalseOperand)

    addSuccessor(targetTrue)
    addSuccessor(targetFalse)
}

//-----------------------------------------------------------------------------//

fun Block.invoke(def: Variable?, vararg uses: Operand) {
    val inst = instruction(Opcode.invoke)
    uses.forEach(inst::addUse)
    if (def != null) inst.addDef(def)
}

//-----------------------------------------------------------------------------//

fun Block.isLastInstructionTerminal(): Boolean
    = instructions.isNotEmpty() && instructions.last().opcode.isTerminal()

//--- Function ----------------------------------------------------------------//

fun Function.newBlock(name: String = "block") = Block(genBlockName(name))
fun Function.addValueParameters(parameters: List<Variable>) { this.parameters.addAll(parameters) }

//--- Ir ----------------------------------------------------------------------//

fun Ir.addKlass(klass: Klass)          { klasses[klass.name] = klass }
fun Ir.addFunction(function: Function) { functions[function.name] = function }

//--- Utilities ---------------------------------------------------------------//

fun Opcode.isTerminal() = this == Opcode.br || this == Opcode.ret || this == Opcode.invoke || this == Opcode.resume

//-----------------------------------------------------------------------------//
// Build direct-ordered list of blocks in graph starting with "enter" block

fun search(enter: Block): List<Block> {
    val result  = mutableListOf<Block>()
    val visited = mutableSetOf<Block>()
    val workSet = mutableListOf(enter)

    while (workSet.isNotEmpty()) {
        val block = workSet.last()

        visited.add(block)
        val successors = block.successors.filterNot { visited.contains(it) }
        workSet.addAll(successors)
        if (successors.isNotEmpty()) continue

        result.add(block)
        workSet.remove(block)
    }
    return result
}

