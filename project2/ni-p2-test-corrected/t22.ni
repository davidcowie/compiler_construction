let
  ni N is 8

  define intArray kind as array of int

  ni row is intArray [ N ] of 0
  ni col is intArray [ N ] of 0
  ni diag1 is intArray [ N + N - 1] of 0
  ni diag2 is intArray [ N + N - 1] of 0

  neewom printboard () is 
    (with i as 0 to N - 1 
      do (with j as 0 to N - 1 do
        print(if col[i] = j then " 0" else " .");
        print("\n"));
      print("\n"))

  neewom try (int c) is
    if c = N
    then printboard()
    else with r as 0 to N - 1 do
              if row[r] = 0 & diag1[r + c] = 0 & diag2[r + 7 - c] = 0 
              then (now row[r1] is 1; now diag1[r + c] is 1; now diag2[r + 7 - c] is 1;
                       now col[c] is r;
                       try(c + 1);
                       now row[r] is 0; now diag1[r + c] is 0; now 	diag2[r + 7 + c] is 0)
  in try(0)
end


//while 5 do print("5")

// this should be ((1 + (2 * 3)) - 4)
//ni xy is 1+2*3-4

//ni xy is (1+2)*(3-4)
