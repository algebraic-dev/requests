import Lake
open Lake DSL

package «requests» where
  -- add package configuration options here

lean_lib «Requests» where
  -- add library configuration options here

require S2ocket from git "https://github.com/KislyjKisel/Socket.lean.git"
require soda from "git@github.com:algebraic-sofia/soda.git"
