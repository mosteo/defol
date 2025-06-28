with Ada.Exceptions;
with Ada.Task_Identification;
with Ada.Task_Termination;

package Defol_Termination is

   protected Termination is

      procedure Handler
        (Cause : Ada.Task_Termination.Cause_Of_Termination;
         T     : Ada.Task_Identification.Task_Id;
         X     : Ada.Exceptions.Exception_Occurrence);

   end Termination;

end Defol_Termination;
