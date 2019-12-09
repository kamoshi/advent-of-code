package intcode

sealed trait State
case object Ready extends State     // machine ready/working
case object Input extends State     // machine waiting for input (empty inputStream)
case object Finished extends State  // machine reached instruction 99
