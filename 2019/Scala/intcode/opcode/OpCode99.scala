package intcode.opcode

/**
 * Marks the end of a program
 */
case object OpCode99 extends OpCode {
  /** length of an instruction */
  override val length: Int = 1
}
