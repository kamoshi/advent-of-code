package intcode

sealed trait State
case object Ready extends State
case object Input extends State
case object Output extends State
case object Finished extends State
