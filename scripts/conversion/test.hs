file <- readFile "input.auto.md" 
let repd = readSmsnLines file
let assignedContent = assignFiles $ repd
