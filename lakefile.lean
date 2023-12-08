import Lake
open Lake DSL

package «requests» where
  -- add package configuration options here

lean_lib «Requests» where
  -- add library configuration options here

require Socket from git "https://github.com/KislyjKisel/Socket.lean.git"
require soda from "https://github.com/algebraic-sofia/soda.git"
