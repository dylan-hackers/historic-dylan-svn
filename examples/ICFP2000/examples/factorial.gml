{ /self /n
  n 2 lessi
     { 1 }
     { n 1 subi self self apply n muli }
  if
} /fact

12 fact fact apply
