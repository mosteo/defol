with Simple_Logging;

package body Defol_Termination is

   package SL renames Simple_Logging;

   protected body Termination is

      -------------
      -- Handler --
      -------------

      procedure Handler
        (Cause : Ada.Task_Termination.Cause_Of_Termination;
         T     : Ada.Task_Identification.Task_Id;
         X     : Ada.Exceptions.Exception_Occurrence)
      is
         use Ada.Task_Identification;
         use Ada.Task_Termination;
      begin
         case Cause is
            when Normal => null;
            when Abnormal =>
               SL.Error ("Task " & Image (T) & " ended abnormally");
            when Unhandled_Exception =>
               SL.Error ("Task " & Image (T)
                         & " ended due to unhandled exception:");
               SL.Error (Ada.Exceptions.Exception_Information (X));
         end case;
      end Handler;

   end Termination;

end Defol_Termination;
