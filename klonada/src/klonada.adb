with Ada.Text_IO; -- Use package Ada. Text_IO
with Ada.Characters.Latin_1; -- Contains the ESC character

use Ada.Text_IO; -- Integrate its namespace
use Ada.Characters.Latin_1;

procedure Klonada is
   Message    : constant String := "Hello World ";
   --- "inspired" by https://github.com/harrisonturton/ada-pretty-print/ ---
   Red_Code   : aliased constant String := ESC & "[31m";
   Reset_Code : String := ESC & "[0m";
begin
   Put_Line (Red_Code & Message & Reset_Code);
end Klonada;