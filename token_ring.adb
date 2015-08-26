--
-- Liam M Gooding, 2015
--

pragma Initialize_Scalars;

with Ada.Real_Time;       use Ada.Real_Time;
with Ada.Text_IO;         use Ada.Text_IO;

package body Token_Ring is

-- always write something to catch exceptions because otherwise these die quietly... alone... and scared
   task body Node_Task is
      Next_Node : Node_Ptr;
      Worker : Worker_Ptr; -- address to the linked task to process the token
      Node_Id : Task_Index;
      Local_Data_Token : Data_Token_Type;
      Local_Status_Token : Status_Token_Type;
      Start_Time, Finish_Time : Time;
      Interval : constant Duration := 0.1; -- seconds 
      Next_Time : Time;

   begin
      Next_Time := Clock + To_Time_Span (Interval);
      loop
         select
            accept Initialise (In_Node : Node) do
               Next_Node := In_Node.Next;
               Node_Id := In_Node.ID;
               Worker := In_Node.Status_Worker;
            end Initialise;
         or
            accept Receive_Status (Status_Token : Status_Token_Type) do
               Local_Status_Token := Status_Token;
               Put_Line ("Node" & Task_Index'Image (Node_Id) & " has received the STATUS token");
            end Receive_Status;

            Start_Time := Clock; -- Clock is function def in Real-time package that returns the time.
            delay until Next_Time; -- pretend like we're doing something
            Next_Time := Clock + To_Time_Span (Interval);
            Finish_Time := Clock;

            Put_Line ("Status token has been held by Node " & Task_Index'Image (Node_Id) & " for " & Duration'Image (To_Duration (Finish_Time - Start_Time)));

            Next_Node.all.Receive_Status (Local_Status_Token);
            -- need to measure how long the node is holding onto the status token for
            -- the delay of this should not be influenced by the data token processing
         or
            -- being sent a token from the previous node
            -- pass it off to the worker task
            accept Send_Data (Data_Token : Data_Token_Type) do
               Put_Line ("Node" & Task_Index'Image (Node_Id) & " has received the DATA token");
               Local_Data_Token := Data_Token;
            end Send_Data;
            Worker.all.Handle_Token (Local_Data_Token);
         or
            -- The worker task is finished processing the data token and returns it via this function
            accept Receive_Data_Token (Data_Token : Data_Token_Type) do
               Local_Data_Token := Data_Token;
            end Receive_Data_Token;
            Next_Node.all.Send_Data (Data_Token => Local_Data_Token);
         end select;
      end loop;
   end Node_Task;

   task body Worker_Task is
      Local_Data_Token : Data_Token_Type;
      Parent_Node : Node_Ptr;
      Interval : constant Duration := 0.5; -- seconds 
      Next_Time : Time;
   begin
      Next_Time := Clock + To_Time_Span (Interval);
      loop
         select
            accept Set_Parent_Task (Parent_Task : Node_Ptr) do
               Parent_Node := Parent_Task;
            end Set_Parent_Task;
         or
            accept Handle_Token (Token : Data_Token_Type) do
               -- Put_Line ("Sub-Task has received the DATA token. Processing and returning to the parent node");
               Local_Data_Token := Token;
            end Handle_Token;
            
            delay until Next_Time; -- simulate handling the token
            Next_Time := Clock + To_Time_Span (Interval);

            Parent_Node.all.Receive_Data_Token (Local_Data_Token); -- .all is the dereference
         end select;
      end loop;
   end Worker_Task;

end Token_Ring;
