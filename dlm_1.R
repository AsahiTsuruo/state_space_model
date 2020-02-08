cat( "\014" )
if( dev.cur() > 1 ) dev.off()
rm( list = ls( envir = globalenv() ), envir = globalenv() )

