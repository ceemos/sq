--  --------------------------------------------------------------------------
--  @Implementation: stacks.adb
--
--   Copyright (c) 2000 Universitaet Stuttgart
--   All rights reserved.
--
--  @Project:
--    Programmierkurs, Programming example
--  @Version:
--    $Revision: 1.3 $
--  @Created:
--    2000-11-06
--  @Author:
--    Members of the Programmierkurs organization team
--
--  @Environment:
--    Gnat 3.12p, gcc 2.8.1, glibc 2.1.3, Linux 2.2.15
--
--  @Description:
--    This package provides a standard stack data type (LIFO-Container).
--  @Implementation:
--    The stack is implemented as single linked, downward oriented list.
--    The reference the user holds always points to the top element of the
--    stack. Every element stores a reference to its predecessor and its
--    number. The number each item stores is equal to its position in the
--    stack.
--
--  @Modifications:
--    $Log: stacks.adb,v $
--    Revision 1.3  2002/11/20 14:43:44  hampptn
--    Empty Lines adjusted
--
--    Revision 1.2  2001/11/06 09:06:45  hampptn
--    Space before pharanthesis adjusted (see Styleguide)
--
--    Revision 1.1  2001/11/06 09:02:09  hampptn
--    Initial revision
--
--    Revision 1.1  2000/11/10 10:37:45  reissing
--    Initial revision
--
------------------------------------------------------------------------------

with Ada.Unchecked_Deallocation;  -- generic deallocation method

package body Stacks is

   --  -----------------------------------------------------------------------
   --  @Procedure: Deallocate
   --
   --  @Description:
   --   Instantiation of the generic Ada operation for freeing allocated
   --   memory on the heap.
   --  @References:
   --   Ada95 Reference Manual 13.11.2

   procedure Deallocate is new
     Ada.Unchecked_Deallocation (Stack_Element, Stack_Type);

   --  -----------------------------------------------------------------------
   --  @Function: Create
   --
   --  @Description:
   --    Initializes the stack as empty.
   --  @Implementation:
   --    Memory is allocated for the first element. This element is tagged
   --    as empty. This is signified by Item_Number = 0.

   function Create return Stack_Type is
      --  temporary object, needed for memory allocation and initialization.
      Element_Access : Stack_Type;
   begin
      Element_Access             := new Stack_Element;
      Element_Access.Item_Number := 0;
      return Element_Access;
   end Create;

   --  -----------------------------------------------------------------------
   --  @Procedure: Destroy
   --
   --  @Description:
   --   Removes the whole stack from the heap.
   --  @Implementation:
   --   All elements in the stack are deallocated (reverse insertion order)

   procedure Destroy (Stack : in out Stack_Type) is
      --  temporary object, needed as cursor through the linked list.
      Element_Access : Stack_Type;
   begin
      pragma Assert (Stack /= null);
      if (Stack /= null) then
         while (Stack /= null) loop
            --  Store reference to previous object and deallocate the
            --  current, after deallocation the cursor is set
            --  to the stored previous element.
            Element_Access := Stack.Previous;
            Deallocate (Stack);
            Stack := Element_Access;
         end loop;
      end if;
      pragma Assert (Stack = null);
   end Destroy;

   --  -----------------------------------------------------------------------
   --  @Procedure: Push
   --
   --  @Description:
   --    Puts a new element on top of the stack.
   --  @Implementation:
   --    Memory is allocated for the new element and the previous reference
   --    of the new element is set to point to the current top element. After
   --    this, the new element is set to be the actual top of the stack.

   procedure Push (Stack : in out Stack_Type;
                   Item  : in Character) is
      --  temporary object, needed for reordering of references while
      --  inserting a new item.
      New_Stack : Stack_Type;
   begin
      pragma Assert (Stack /= null);
      if (Stack /= null) then
         if (Stack.Item_Number = 0) then
            Stack.Item_Number := 1;
            Stack.Item        := Item;
         else
            New_Stack             := new Stack_Element;
            New_Stack.Item        := Item;
            --  Each Item holds its own number, which is equal to its
            --  position the stack.
            New_Stack.Item_Number := Stack.Item_Number + 1;
            New_Stack.Previous    := Stack;
            Stack                 := New_Stack;
         end if;
      end if;
      pragma Assert (Top (Stack) = Item);
   end Push;

   --  -----------------------------------------------------------------------
   --  @Procedure: Pop
   --
   --  @Description:
   --   Removes the element on top of the stack.
   --  @Implementation:
   --   There are two states of the stack that must be handled separately. If
   --   the stack has more than one element, the element on top is removed
   --   by making the previous element the new top element and deallocating
   --   the former top element. If only one element is on the stack,
   --   the item_number is set to zero to signal that the stack is empty.
   --   Thus, the first element on the stack is never removed - only by
   --   destroy.

   procedure Pop (Stack : in out Stack_Type) is
      Element_Reference : Stack_Type;
   begin
      pragma Assert (Stack /= null);
      if (Stack /= null) then
         if (Stack.Item_Number >= 2) then
            --  More than one elements on the Stack
            Element_Reference := Stack.Previous;
            Deallocate (Stack);
            Stack := Element_Reference;
         elsif (Stack.Item_Number = 1) then
            --  The first element
            Stack.Item_Number := 0;
         end if;
      end if;
   end Pop;

   --  -----------------------------------------------------------------------
   --  @Function: Top
   --
   --  @Description:
   --    Returns the element on top of the stack.
   --  @Error Handling:
   --    If the stack is empty, there is no value to return. In this case
   --    the Empty_Stack_Access exception is thrown.

   function Top (Stack : in Stack_Type) return Character is
   begin
      if Is_Empty (Stack) then
         --  Nothing useful can be returned if the stack is empty!
         raise Empty_Stack_Access;
      else
         return Stack.Item;
      end if;
   end Top;

   --  -----------------------------------------------------------------------
   --  @Function: Size
   --
   --  @Description:
   --    Returns the number of elements stored on the stack.
   --  @Implementation:
   --    Each element has a item_number which is equal to the current size.
   --    Out of this, only the item_number needs to be returned.

   function Size (Stack : in Stack_Type) return Natural is
   begin
      pragma Assert (Stack /= null);
      if (Stack /= null) then
         return Stack.Item_Number;
      else
         return 0;
      end if;
   end Size;

   --  -----------------------------------------------------------------------
   --  @Function: Is_Empty
   --
   --  @Description:
   --    Returns true if the stack is empty.
   --  @Implementation:
   --    If item_number equals 0, it signals that the stack is empty.

   function Is_Empty (Stack : in Stack_Type) return Boolean is
   begin
      pragma Assert (Stack /= null);
      return ((Stack = null) or else (Stack.Item_Number = 0));
   end Is_Empty;

end Stacks;
