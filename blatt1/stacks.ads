--  --------------------------------------------------------------------------
--  @Specification: stacks.ads
--
--   Copyright (c) 2000 Universitaet Stuttgart
--   All rights reserved.
--
--  @Project:
--    Programmierkurs, Programming example
--  @Version:
--    $Revision: 1.4 $
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
--  @Usage:
--    Use create to initialize a new stack and destroy to remove it from
--    the heap. Between this two calls the stack can be accessed through
--    the other declared functions.
--  @Restrictions:
--    The element type Item is set to Character,
--    however, this can be changed easily
--  @Error Handling:
--    If an empty stack is accessed by Top, the exception
--    Empty_Stack_Access is raised. If an empty stack
--    is accessed by other functions, nothing will happen.
--
--  @Modifications:
--    $Log: stacks.ads,v $
--    Revision 1.4  2002/11/20 14:46:14  hampptn
--    Empty line inserted
--
--    Revision 1.3  2002/11/20 14:45:11  hampptn
--    Empty lines adjusted
--
--    Revision 1.2  2001/11/06 09:04:06  hampptn
--    Space before pharanthesis adjusted (see Styleguide)
--
--    Revision 1.1  2001/11/06 09:01:33  hampptn
--    Initial revision
--
--    Revision 1.1  2000/11/10 10:37:32  reissing
--    Initial revision
--
--  --------------------------------------------------------------------------

package Stacks is

   --  -----------------------------------------------------------------------
   --  @Abstract Data Type: Stack_Type
   --
   --  @Purpose:
   --    This type represents a stack
   --
   --  @Methods:
   --    Create   - creates an empty stack
   --    Destroy  - destroy a stack
   --    Push     - pushes an element on top of the stack
   --    Pop      - removes the top element from the stack
   --    Top      - returns the top element of the stack
   --    Size     - returns the size of the stack (no. of elements)
   --    Is_Empty - checks if stack is empty (no elements)

   type Stack_Type is private;

   --  -----------------------------------------------------------------------
   --  @Exception: Empty_Stack_Access
   --
   --  @Purpose:
   --    This exception is thrown, if an empty stack is accessed by
   --    the Top operation.
   Empty_Stack_Access : exception;

   --  -----------------------------------------------------------------------
   --  @Function: Create
   --
   --  @Description:
   --    Initializes a stack object before the first use. The stack is
   --    initialized as empty.
   --  @Usage:
   --    This method must be used before any other method accessing the
   --    stack.
   --  @Return:
   --    A new Stack object.

   function Create return Stack_Type;

   --  -----------------------------------------------------------------------
   --  PROCEDURE: Destroy
   --
   --  @Description:
   --    Removes the whole stack from the heap and deinitializes the
   --    stack.
   --  @Usage:
   --    Stack must have been created previously
   --  @Parameter:
   --    Stack - Stack object.

   procedure Destroy (Stack : in out Stack_Type);

   --  -----------------------------------------------------------------------
   --  PROCEDURE: Push
   --
   --  @Description:
   --    Places a new item on the top of the stack.
   --  @Usage:
   --    Stack must have been created previously
   --  @Parameter:
   --    Stack - the stack object.
   --    Item  - the item to place on top of the stack.

   procedure Push (Stack : in out Stack_Type;
                   Item  : in Character);

   --  -----------------------------------------------------------------------
   --  PROCEDURE: Pop
   --
   --  @Description:
   --    Removes the top item from the stack.
   --  @Usage:
   --    Stack must have been created previously
   --  @Parameter:
   --    Stack - The stack object.

   procedure Pop (Stack : in out Stack_Type);

   --  -----------------------------------------------------------------------
   --  @Function: Top
   --
   --  @Description:
   --    Returns the item stored on top of the stack.
   --  @Usage:
   --    Stack must have been created previously
   --  @Parameter:
   --    Stack - the stack object.
   --  @Return:
   --    Item on top of the stack.
   --  ERROR HANDLING:
   --    If the stack object is empty, the exception
   --    Empty_Stack_Access is raised.

   function Top (Stack : in Stack_Type) return Character;

   --  -----------------------------------------------------------------------
   --  @Function: Size
   --
   --  @Description:
   --    Returns the number of elements stored on the stack.
   --  @Usage:
   --    Stack must have been created previously
   --  @Parameter:
   --    Stack - The stack object.
   --  @Return:
   --    Number of elements stored.

   function Size (Stack : in Stack_Type) return Natural;

   --  -----------------------------------------------------------------------
   --  @Function: Is_Empty
   --
   --  @Description:
   --    Checks if the stack is empty or filed.
   --  @Usage:
   --    Stack must have been created previously
   --  @Parameter:
   --    Stack - The stack object.
   --  @Return:
   --    True  - if the stack is empty
   --    False - if the stack isn't empty.

   function Is_Empty (Stack : in Stack_Type) return Boolean;

private

   --  -----------------------------------------------------------------------
   --  DATA TYPE: Stack_Element
   --
   --  PURPOSE:
   --    This record represents the data structure to store items on the
   --    stack.
   --  ELEMENTS:
   --    Item - stores the value of the stored item
   --    Item_Number - stores the number of the stored element. The number
   --      of the stored element equals the size of the stack.
   --    Previous - Points to the previous element on the stack.

   type Stack_Element is record
      Item        : Character;
      Item_Number : Natural;
      Previous    : Stack_Type;
   end record;

   type Stack_Type is access Stack_Element;

end Stacks;
