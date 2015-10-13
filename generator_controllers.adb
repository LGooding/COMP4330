--
-- Uwe R. Zimmer, Australia 2015
--

with Ada.Interrupts.Names;                use Ada.Interrupts.Names;
with Discovery_Board;                     use Discovery_Board;
with Discovery_Board.LED_Interface;       use Discovery_Board.LED_Interface;
with STM32F4;                             use type STM32F4.Bit, STM32F4.Bits_32;
with STM32F4.Timers.Ops;                  use STM32F4.Timers.Ops;
with STM32F4.Reset_and_clock_control.Ops; use STM32F4.Reset_and_clock_control.Ops;
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
            Discovery_Board.LED_Interface.All_Off;
            State := Off;
         else 
            Discovery_Board.LED_Interface.All_On;
            State := On;
         end if;
         STM32F4.Timers.Ops.Clear_Flag (No => 2, This_Flag => Update);
         -- toggle LEDs for time being, later output signal

      end Interrupt_Handler;

   end Timer_Update;

   function Current_Oscillator_State return Oscillator_State is (Timer_Update.Current_State);

   procedure Initialize is

   begin
      STM32F4.Reset_and_clock_control.Ops.Enable (No => 2);
      STM32F4.Timers.Ops.Enable (No => 2);
      STM32F4.Timers.Ops.Set_Auto_Reload_32 (No => 2, Auto_Reload => 16_000_000);
      STM32F4.Timers.Ops.Enable (No  => 2, Int => Update);

      -- need to set the system to allow the interrupt as well, pin 28 of the vector table

   end Initialize;

begin
   Initialize;
end Generator_Controllers;
