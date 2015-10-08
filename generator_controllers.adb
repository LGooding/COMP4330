--
-- Uwe R. Zimmer, Australia 2015
--

with Ada.Interrupts.Names;          use Ada.Interrupts.Names;
with Ada.Real_Time;                 use Ada.Real_Time;
with ANU_Base_Board;                use ANU_Base_Board;
with ANU_Base_Board.Com_Interface;  use ANU_Base_Board.Com_Interface;
with ANU_Base_Board.LED_Interface;  use ANU_Base_Board.LED_Interface;
with Discovery_Board;               use Discovery_Board;
with Discovery_Board.Button;        use Discovery_Board.Button;
with Discovery_Board.LED_Interface; use Discovery_Board.LED_Interface;
with STM32F4;                       use type STM32F4.Bit, STM32F4.Bits_32;
with System;                        use System;

with STM32F4.Random_number_generator.Ops; use STM32F4.Random_number_generator.Ops;
with STM32F4.Reset_and_clock_control.Ops; use STM32F4.Reset_and_clock_control.Ops;
with STM32F4.Timers.Ops;                  use STM32F4.Timers.Ops;

package body Generator_Controllers is

   protected Timer_Underflow with Interrupt_Priority => Interrupt_Priority'First is

      function Current_State return Oscillator_State;

   private
      procedure Interrupt_Handler with Attach_Handler => TIM2_Interrupt;
      -- attempting to match this interrupt with the hardware set flag, probably wrong
      pragma Unreferenced (Interrupt_Handler);

      State     : Oscillator_State := Off;
   end Timer_Underflow;

   protected body Timer_Underflow is

      function Current_State return Oscillator_State is (State);

      procedure Interrupt_Handler is

      begin
         Clear_Flag (No => 2, This_Flag => Update); -- clearing the interrupt so others can be triggered
         State     := (if State = On then Off else On);

         if State = On then
            Discovery_Board.LED_Interface.All_On;
         else
            Discovery_Board.LED_Interface.All_Off;
         end if;
         
      end Interrupt_Handler;

   end Timer_Underflow;

end Generator_Controllers;
