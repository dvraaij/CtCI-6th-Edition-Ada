------------------------------------------------------------------------------
--                                                                          --
--                           CtCI Code Library                              --
--                                                                          --
--               C t C I . T r e e _ N o d e . E x p o r t                  --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
-- This package is used in chapter 4. It contains functions to export the   --
-- graph to external formats, mainly for visualization. Currently only      --
-- one file-format is supported: LaTeX / TikZ.                              --
--                                                                          --
-- This package is meant for use in solutions of the execises in the book   --
-- "Cracking the Coding Interview" and should not be used in production     --
-- software. Use the appropriate containers from the Ada Standard Library   --
-- instead.                                                                 --
--                                                                          --
------------------------------------------------------------------------------

package CtCI.Tree_Node.Export is

   -- Export tree to TikZ code
   procedure To_TikZ (TN : Tree_Node_Access; File_Name : String);

end CtCI.Tree_Node.Export;
