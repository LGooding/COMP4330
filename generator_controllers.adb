--
-- Liam M. Gooding, Australia 2015
--

with Ada.Interrupts.Names;                        use Ada.Interrupts.Names;
with ANU_Base_Board.Com_Interface;                use ANU_Base_Board.Com_Interface;
with ANU_Base_Board.LED_Interface;                use ANU_Base_Board.LED_Interface;
with Discovery_Board;                             use Discovery_Board;
with Discovery_Board.LED_Interface;               use Discovery_Board.LED_Interface;
with STM32F4;                                     use type STM32F4.Bit, STM32F4.Bits_32;
with STM32F4.General_purpose_IOs;                 use STM32F4.General_purpose_IOs;
with STM32F4.Timers.Ops;                          use STM32F4.Timers.Ops;
with STM32F4.Reset_and_clock_control.Ops;         use STM32F4.Reset_and_clock_control.Ops;
with STM32F4.Interrupts_and_Events.Ops;           use STM32F4.Interrupts_and_Events.Ops;
with STM32F4.System_configuration_controller.Ops; use STM32F4.System_configuration_controller.Ops;
with System;                                      use System;

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

   protected Input_Edge_Event with Interrupt_Priority => Interrupt_Priority'First is

   private
      procedure Interrupt_Handler;
      -- two of the COM Ports use the same external interrupt line :/ will this be a problem?
      pragma Attach_Handler (Interrupt_Handler, EXTI9_5_Interrupt);   -- COM Port 1
      pragma Attach_Handler (Interrupt_Handler, EXTI9_5_Interrupt);   -- COM Port 2
      pragma Attach_Handler (Interrupt_Handler, EXTI15_10_Interrupt); -- COM Port 3
      pragma Attach_Handler (Interrupt_Handler, EXTI15_10_Interrupt); -- COM Port 3

      pragma Unreferenced (Interrupt_Handler);
   end Input_Edge_Event;

   protected body Input_Edge_Event is

      procedure Interrupt_Handler is
         -- read the line to determine which port triggered the interrupt -> how?
         -- use left LED for transmit indication, right LED for receive
      begin
         ANU_Base_Board.LED_Interface.On (LED => (1, Left)); -- For COM port 1

      end Interrupt_Handler;

   end Input_Edge_Event;

   function Current_Oscillator_State return Oscillator_State is (Timer_Update.Current_State);

   procedure Initialize is

   begin

      -- Setting up the Oscillator
      STM32F4.Reset_and_clock_control.Ops.Enable (No => 2);
      STM32F4.Timers.Ops.Enable (No => 2);
      STM32F4.Timers.Ops.Set_Auto_Reload_32 (No => 2, Auto_Reload => 16_000_000); -- counting up is the default, need to change this, hear that the manual reccomends low prescaler value and high clock division
      STM32F4.Timers.Ops.Enable (No => 2, Int => Update); -- what interrupts should be masked for this?

      -- Enabling the GPIO ports attached to the COM Ports
      Enable (B);
      Enable (C);
      Enable (D);

      Enable (System_Configuration_Contr); -- not sure what the function of this line is

      -- Aligning the Rx lines to allow interrupts to trigger on input
      Set_Interrupt_Source (Interrupt_No => 7,  Port => B);  -- COM Port 1, not sure if these are called correctly, look at the button example, also should change to reference not a magic number
      Set_Interrupt_Source (Interrupt_No => 6,  Port => D);  -- COM Port 2
      Set_Interrupt_Source (Interrupt_No => 11, Port => C);  -- COM Port 3
      Set_Interrupt_Source (Interrupt_No => 2,  Port => D);  -- COM Port 4

      -- Need to know where falling edge is to give buffer area for inserting UART signal 
      Set_Trigger (Line => 7,  Raising => Enable, Falling => Enable);
      Set_Trigger (Line => 6,  Raising => Enable, Falling => Enable);
      Set_Trigger (Line => 11, Raising => Enable, Falling => Enable);
      Set_Trigger (Line => 2,  Raising => Enable, Falling => Enable);

      -- Interrupts aren't masked so that they can stack, which is what I want... I think 
      Masking (Line => 7,  State => Unmasked);
      Masking (Line => 6,  State => Unmasked);
      Masking (Line => 11, State => Unmasked);
      Masking (Line => 2,  State => Unmasked);

   end Initialize;

begin
   Initialize;
end Generator_Controllers;
