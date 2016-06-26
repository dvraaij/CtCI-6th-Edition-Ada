------------------------------------------------------------------------------
--                                                                          --
--                           CtCI Code Library                              --
--                                                                          --
--               C t C I . T r e e _ N o d e . E x p o r t                  --
--                                                                          --
--                                 B o d y                                  --
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

with Ada.Text_IO;         use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;

package body CtCI.Tree_Node.Export is

   ----------------------------------------------------------------------------
   procedure To_TikZ (TN : Tree_Node_Access; File_Name : String) is

      -------------------------------------------------------------------------
      procedure Put
        (File   : File_Type       ;
         TN     : Tree_Node_Access;
         Indent : Positive_Count  )
      is
      begin

         New_Line (File);
         Set_Col (File, Indent);

         if (TN = null) then
            Put (File, "child[missing]");
         else
            Put (File, "child {node [circle,draw] {");
            Put (File, TN.Data , 0);
            Put (File, "}");
            Put (File, TN.Left , Indent + 2);
            Put (File, TN.Right, Indent + 2);
            Put (File, "}");
         end if;

      end Put;

      File : File_Type;

   begin

      -- Create file
      Create (File, Out_File, File_Name);

      -- Add preamble
      Put_Line (File, "\documentclass{standalone}"                                     );
      Put_Line (File, "\usepackage{tikz}"                                              );
      Put_Line (File, "\begin{document}"                                               );
      Put_Line (File, "  \sffamily"                                                    );
      Put_Line (File, "  \begin{tikzpicture}[level/.style={sibling distance=60mm/#1}]" );

      -- Add root node and its chilren
      Set_Col (File, 5);

      Put (File, "\node [circle,draw] {");
      Put (File, TN.Data , 0);
      Put (File, "}");
      Put (File, TN.Left , 7);
      Put (File, TN.Right, 7);
      Put (File, ";");

      New_Line (File);

      -- Add epiloque
      Put_Line (File,"  \end{tikzpicture}" );
      Put_Line (File,"\end{document}"      );

      -- Close file
      Close (File);

   end To_TikZ;

end CtCI.Tree_Node.Export;
