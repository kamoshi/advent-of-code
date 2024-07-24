import Lake
open Lake DSL

package «aoc2015» where
  -- add package configuration options here

lean_lib «Aoc2015» where
  -- add library configuration options here

@[default_target]
lean_exe «aoc2015» where
  root := `Main
