let
  define indexedString kind as { int index, string value }
  define stringarray kind as array of indexedString
  ni i is indexedString { index is 5, value is "hello world" }
  ni j is stringarray[i.index + 10] of 3
in
  (now j[0] is indexedString { index is 2, value is "bo" }; 0 )
end
