generic
package Defol.Deleting is

   --  Task that performs file/folder deletions from the deletion queue

   task Deleter is
      entry Start;
      --  This allows postponing deletions after matching

      entry Done;
   end Deleter;

end Defol.Deleting;
