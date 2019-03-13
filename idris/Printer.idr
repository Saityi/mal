module Printer

import Types

export
printMalSexp : MalSexp -> String
printMalSexp = show