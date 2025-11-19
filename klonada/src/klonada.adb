with Ada.Text_IO; -- Use package Ada. Text_IO
with Ada.Characters.Latin_1; -- Contains the ESC character
with Ada.Containers.Vectors;
with Ada.Containers.Indefinite_Hashed_Maps;
with Ada.Strings.Hash;
with Ada.Strings.Unbounded;

with Ada.Numerics.Discrete_Random;
use Ada.Strings.Unbounded;
use Ada.Text_IO; -- Integrate its namespace
use Ada.Characters.Latin_1;

procedure Klonada is

   --- TYPES ---
   type Color_Type is (Black, Red);
   type Suit_Type is (Clubs, Diamonds, Hearts, Spades);

   subtype Rank_Type is Integer with Static_Predicate => Rank_Type in 1 .. 13;

   -- Card string example: "[H13]", "[C1 ]"
   type Card_Type is record
      Suit    : Suit_Type;
      Rank    : Rank_Type;
      Face_Up : Boolean;
   end record;

   type Position is record
      Pile_Index   : Integer;
      Pile_Pointer : Integer;
   end record;

   package Card_Vectors is new
     Ada.Containers.Vectors (Index_Type => Natural, Element_Type => Card_Type);

   subtype Pile_Type is Card_Vectors.Vector;
   use type Card_Vectors.Vector;
   type Pile_Ptr is access all Pile_Type;

   type Pile_List is array (Positive range <>) of Pile_Ptr;

   package Pile_Maps is new
     Ada.Containers.Indefinite_Hashed_Maps
       (Key_Type        => String,
        Element_Type    => Pile_Type,
        Hash            => Ada.Strings.Hash,
        Equivalent_Keys => "=");

   use Pile_Maps; -- why?

   --  function Is_Valid_Move(Selected_Destination: Destination)
   --- VARIABLES ETC ---
   Pile1 : aliased Pile_Type;
   Pile2 : aliased Pile_Type;
   Pile3 : aliased Pile_Type;
   Pile4 : aliased Pile_Type;
   Pile5 : aliased Pile_Type;
   Pile6 : aliased Pile_Type;
   Pile7 : aliased Pile_Type;

   Foundations : Pile_List (1 .. 7) :=
     (Pile1'Access,
      Pile2'Access,
      Pile3'Access,
      Pile4'Access,
      Pile5'Access,
      Pile6'Access,
      Pile7'Access);

   Clubs_Pile    : Pile_Type;
   Hearts_Pile   : Pile_Type;
   Diamonds_Pile : Pile_Type;
   Spades_Pile   : Pile_Type;

   Draw_Pile    : Pile_Type;
   Discard_Pile : Pile_Type;

   Selected_Card        : Card_Type;
   Selected_Destination : Position;

   Pile_Map : Map;

   Message    : constant String := "Hello World ";
   Red_Code   : constant String := ESC & "[31m";
   Reset_Code : constant String := ESC & "[0m";

   --- FUNCTIONS AND PROCEDURES ---
   function Get_Color (S : Suit_Type) return Color_Type is
   begin
      return
        (case S is
           when Clubs | Spades    => Black,
           when Diamonds | Hearts => Red);
   end Get_Color;

   function To_String (S : Suit_Type) return String is
   begin
      case S is
         when Hearts   =>
            --  return "♡";
            return "H";

         when Diamonds =>
            --  return "♢";
            return "D";

         when Clubs    =>
            --  return "♧";
            return "C";

         when Spades   =>
            --  return "♤";
            return "S";
      end case;
   end To_String;

   function To_String (R : Rank_Type) return String is
   begin
      case R is
         when 13     =>
            return "K ";

         when 12     =>
            return "Q ";

         when 11     =>
            return "J ";

         when 10     =>
            return R'Image;

         when 1 .. 9 =>
            return R'Image & " ";
      end case;
   end To_String;

   -- eg. [K ♡] [10♤]
   function To_String (C : Card_Type) return String is
   begin
      declare
         Color_Code  : String :=
           (if Get_Color (C.Suit) = Red then Red_Code else "");
         Rank_Symbol : String := To_String (C.Rank);
         Suit_Symbol : String := To_String (C.Suit);
      begin
         if C.Face_Up then
            return
              "[" & Color_Code & Rank_Symbol & Suit_Symbol & Reset_Code & "]";
         else
            return "[]";
         end if;
      end;
   end To_String;

   function To_String (P : Pile_Type) return String is
   begin
      declare
         S_tmp : Unbounded_String := Null_Unbounded_String;
      begin
         for C of P loop
            S_tmp := S_tmp & To_String (C);
         end loop;
         declare 
            S : String := To_String(S_tmp);
         begin
            return S;
         end;
      end;
   end To_String;

   procedure Pile_Init is
   begin
      Pile_Map.Include ("z", Pile1);
      Pile_Map.Include ("x", Pile2);
      Pile_Map.Include ("c", Pile3);
      Pile_Map.Include ("v", Pile4);
      Pile_Map.Include ("b", Pile5);
      Pile_Map.Include ("n", Pile6);
      Pile_Map.Include ("m", Pile7);
      Pile_Map.Include ("a", Spades_Pile);
      Pile_Map.Include ("s", Diamonds_Pile);
      Pile_Map.Include ("d", Clubs_Pile);
      Pile_Map.Include ("f", Hearts_Pile);
      Pile_Map.Include ("q", Draw_Pile);
      Pile_Map.Include ("w", Discard_Pile);
   end Pile_Init;

   function Create_Deck return Pile_Type is
   begin
      declare
         Deck : Pile_Type;
      begin
         for I in 1 .. 13 loop
            declare
               Club_Card     : Card_Type :=
                 (Suit => Clubs, Rank => I, Face_Up => True);
               Hearts_Card   : Card_Type :=
                 (Suit => Hearts, Rank => I, Face_Up => True);
               Spades_Card   : Card_Type :=
                 (Suit => Spades, Rank => I, Face_Up => True);
               Diamonds_Card : Card_Type :=
                 (Suit => Diamonds, Rank => I, Face_Up => True);
            begin
               Deck.Append (Club_Card);
               Deck.Append (Hearts_Card);
               Deck.Append (Spades_Card);
               Deck.Append (Diamonds_Card);
            end;
         end loop;
         return Deck;
      end;
   end Create_Deck;

   procedure Take_Random_Card (Deck : in out Pile_Type; Card : out Card_Type)
   is
      subtype Random_Range is Integer range 1 .. (Integer (Deck.Length)-1); -- why tho
      package R is new Ada.Numerics.Discrete_Random (Random_Range);
      use R;
      G : Generator;
      X : Random_Range;
   begin
      Reset(G);
      X := Random (G);
      Put_Line ("Picked number: " & X'Image);
      Card := Deck (X);
      Deck.Delete (X);
   end Take_Random_Card;

   procedure Foundations_Init is
      Deck : Pile_Type := Create_Deck;
   begin
      for I in 1..7 loop
         for J in I..7 loop
            declare 
               Card : Card_Type;
            begin
               Take_Random_Card (Deck, Card);
               Foundations(J).all.Append(Card);
            end;
         end loop;
      end loop;
   end Foundations_Init;

begin
   declare
      --  Deck : Pile_Type := Create_Deck;
      --  Removed_Card : Card_Type;
   begin
      Foundations_Init;
      for Foundation_Ptr of Foundations loop
         declare
            Foundation : Pile_Type := Foundation_Ptr.all;
         begin
            --  Put_Line (Foundation.Length'Image);
            Put_Line (To_String(Foundation));
         end;
      end loop;
   end;
end Klonada;
