package body Defol.Matching is

   procedure Match_Pairs is
   begin
      declare
         First  : Item_Ptr;
         Second : Item_Ptr;
      begin
         loop
            Pending_Items.Get (First, Second);
            if First = null and then Second = null then
               exit;
            end if;

            --  Do the matching here
            null;

            --  Debug
            Debug ("Matching: "
                   & First.Path & " :: " & Second.Path);

         end loop;
      end;
      Debug ("Matching completed");
   end Match_Pairs;

end Defol.Matching;
