--
-- Liam M. Gooding, Australia 2015
--

with Ada.Interrupts.Names;                use Ada.Interrupts.Names;
with ANU_Base_Board.Com_Interface;        use ANU_Base_Board.Com_Interface;
with Discovery_Board;                     use Discovery_Board;
with Discovery_Board.LED_Interface;       use Discovery_Board.LED_Interface;
with STM32F4;                             use type STM32F4.Bit, STM32F4.Bits_32;
with STM32F4.Timers.Ops;                  use STM32F4.Timers.Ops;
with STM32F4.Reset_and_clock_control.Ops; use STM32F4.Reset_and_clock_control.Ops;
with STM32F4.Interrupts_and_Events.Ops;   use STM32F4.Interrupts_and_Events.Ops;
with System;                              use System;

package body Generator_Controllers is

   protected Timer_Update with Interrupt_Priority => Interrupt_Priority'First is

      function Current_State return Oscillator_State;

   private
      procedure Interrupt_Handler with Attach_Handler => TIM2_Interrupt;
      pragma Unreferenced (Interrupt_Handler);

      State     : Oscillator_State := Off;
   end Timer_Update;

   protected body Timer_Update is

      function Current_State return Oscillator_State is (State);

      procedure Interrupt_Handler is

      begin
         if State = On then
            Discovery_Board.LED_Interface.On (LED => Red);
            Discovery_Board.LED_Interface.Off (LED => Green);
            -- stop writing to all output pins
            State := Off;
         else
            Discovery_Board.LED_Interface.Off (LED => Red);
            Discovery_Board.LED_Interface.On (LED => Green);
            -- write to all output pins
            State := On;
         end if;
         STM32F4.Timers.Ops.Clear_Flag (No => 2, This_Flag => Update);

      end Interrupt_Handler;

   end Timer_Update;

   protected Incoming_Rising_Edge with Interrupt_Priority => Interrupt_Priority'First is

   private
      procedure Interrupt_Handler with Attach_Handler => EXTI9;
      pragma Unreferenced (Interrupt_Handler);
   end Incoming_Rising_Edge;

   protected body Incoming_Rising_Edge is

      procedure Interrupt_Handler is
         -- flash com lights
         ANU_Base_Board.LED_Interface.On (LED => 1); -- For COM port 1

      end Interrupt_Handler;

   end Incoming_Rising_Edge;


   function Current_Oscillator_State return Oscillator_State is (Timer_Update.Current_State);

   procedure Initialize is

   begin

      -- Setting up the Oscillator
      STM32F4.Reset_and_clock_control.Ops.Enable (No => 2);
      STM32F4.Timers.Ops.Enable (No => 2);
      STM32F4.Timers.Ops.Set_Auto_Reload_32 (No => 2, Auto_Reload => 16_000_000); -- counting up is the default, need to change this, hear that the manual reccomends low prescaler value and high clock division
      STM32F4.Timers.Ops.Enable (No  => 2, Int => Update); -- what interrupts are masked for this?

      -- Setting up the COM Ports to receive and send signals
      STM32F4.Interrupts_and_Events.Ops.Set_Interrupt_Source (Interrupt_No => 9, Port => B); -- just COM Port 1 for the time being


   end Initialize;

begin
   Initialize;
end Generator_Controllers;
