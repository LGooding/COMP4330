--
-- Liam Gooding 2015
--

package Token_Ring is

   type Node;
   type Task_Index is mod 8;

   -- a task to receive the token messages, dispatches contents of token to worker task to handle
   task type Node_Task is
      entry Initialise (In_Node : Node);
      entry Send_Token (Token : Character); -- called by another task to send the token to this node
      entry Receive_Token (Token : Character); -- called by the worker task to return the token to be sent 
   end Node_Task;

   type Node_Ptr is access all Node_Task;

   -- a task to process the tokens away from the message handler
   task type Worker_Task is
      entry Set_Parent_Task (Parent_Task : Node_Ptr);
      entry Handle_Token (Token : Character); -- called by Node_Task to send the token to this task for processing
   end Worker_Task;

   type Worker_Ptr is access all Worker_Task;

   type Node is record
      ID : Task_Index; -- to identify the node
      Next : Node_Ptr; -- directions to the next node
      Status_Worker : Worker_Ptr; -- directions to associated worker task
   end record;

   nodes : array (Task_Index) of aliased Node_Task;
   workers : array (Task_Index) of aliased Worker_Task; -- for the time being just one sub-task because we're only handling one token

end Token_Ring;
