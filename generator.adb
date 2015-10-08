--
-- Uwe R. Zimmer, Australia 2015
--

with Ada.Real_Time;                 use Ada.Real_Time;
with ANU_Base_Board.LED_Interface;  use ANU_Base_Board.LED_Interface;
with Discovery_Board.LED_Interface; use Discovery_Board.LED_Interface;
with Generator_Controllers;         pragma Unreferenced (Generator_Controllers);
with Last_Chance_Handler;           pragma Unreferenced (Last_Chance_Handler);

with STM32F4.Timers.Ops;                   use STM32F4.Timers.Ops;

with System; use System;

procedure Generator with Priority => Priority'First is

   System_Startup : constant Time      := Clock;
   Reset_Flicker  : constant Time_Span := Milliseconds (20);

   package ABBL renames ANU_Base_Board.LED_Interface;
   package DBL  renames Discovery_Board.LED_Interface;

begin

   -- set up timer for generator signal
   STM32F4.Timers.Ops.Enable (2);
   STM32F4.Timers.Ops.Set_Auto_Reload_32 (No => 2, Auto_Reload => 320_000);

   -- set up timer overflow interrupt
   STM32F4.Timers.Ops.Enable (No => 2, Int => Update); -- this means a trigger should trigger on either an overflow or underflow

   -- link timer interrupt to LEDs

   -- All tasks are running at this point.

   -- Briefly flickering all lights to acknowledge a successful start.

   DBL.All_On;
   ABBL.All_On;
   delay until System_Startup + Reset_Flicker;
   DBL.All_Off;
   ABBL.All_Off;

   loop
      null; -- Main task (at lowest priority) needs to be prevented from exiting
   end loop;
end Generator;
