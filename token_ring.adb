--
-- Liam Gooding - u5011681
--

pragma Initialize_Scalars;

with Ada.Text_IO;         use Ada.Text_IO;

package body Token_Ring is

-- always write something to catch exceptions because otherwise these die quietly... alone... and scared
   task body Node_Task is
      Next_Node : Node_Ptr;
      Worker : Worker_Ptr; -- address to the linked task to process the token
      Node_Id : Task_Index;
      Local_Token : Character;

   begin
      loop
         select
            accept Initialise (In_Node : Node) do
               Next_Node := In_Node.Next;
               Node_Id := In_Node.ID;
               Worker := In_Node.Status_Worker;
            end Initialise;
         or
            -- being sent a token from the previous node
            -- pass it off to the worker task
            accept Send_Token (Token : Character) do
               Put_Line ("Node" & Task_Index'Image (Node_Id) & " has received Status Token");
               Local_Token := Token;
            end Send_Token;
            Worker.all.Handle_Token (Local_Token);
         or
            -- The worker task is finished processing the token and returns it via this function
            accept Receive_Token (Token : Character) do
               Local_Token := Token;
            end Receive_Token;
            Next_Node.all.Send_Token (Token => Local_Token);
         end select;
      end loop;
   end Node_Task;

   task body Worker_Task is
      Local_Token : Character;
      Parent_Node : Node_Ptr;
   begin
      loop
         select
            accept Set_Parent_Task (Parent_Task : Node_Ptr) do
               Parent_Node := Parent_Task;
            end Set_Parent_Task;
         or
            accept Handle_Token (Token : Character) do
               Put_Line ("Sub-Task has received the token " & Character'Image (Token) & ". Processing and Sending to the master Node");
               delay 1.0; -- simulate handling the token
               Local_Token := Token;
            end Handle_Token;
            Parent_Node.all.Receive_Token (Local_Token); -- .all is the dereference
         end select;
      end loop;
   end Worker_Task;

end Token_Ring;
