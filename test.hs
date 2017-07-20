file <- readFile "input.auto.md" 
let repd = map (exportedSmsnLine . stripSmsnAddress . stripLeadingSpace) $ lines file
let assignedContent = assignFiles $ repd

