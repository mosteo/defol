with AAA.Strings;

package body Defol.Matching is

   procedure Match_Pairs is
   begin
      declare
         First  : Item_Ptr;
         Second : Item_Ptr;
      begin
         loop
            Pending_Items.Get (First, Second);
            if First = null or else Second = null then
               exit;
            end if;

            --  Skip if one is a prefix of the other
            if AAA.Strings.Has_Prefix (First.Path, Second.Path)
               or else AAA.Strings.Has_Prefix (Second.Path, First.Path) then
               Debug ("Skipping same prefix: "
                      & First.Path & " :: " & Second.Path);
               exit;
            end if;

            if First.Parent = Second.Parent then
               Debug ("Skipping same parent: "
                      & First.Path & " :: " & Second.Path);
               exit;
            end if;

            --  Debug
            Debug ("Matching: "
                   & First.Path & " :: " & Second.Path);

            --  Do the matching here
            if Same_Contents (First, Second) then
               --  TODO: record the matchin their parent dirs pair
               Debug ("IDENTICAL: " & First.Path & " = " & Second.Path);
            end if;

         end loop;
      end;
      Debug ("Matching completed");
   end Match_Pairs;

end Defol.Matching;
