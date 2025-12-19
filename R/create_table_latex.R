create_table_latex <- function(
    df,
    caption,
    filn = "table.tex",
    align = rep("l", (ncol(df)+1)),
    process_citations = FALSE,
    ...
    ){

	latextable <- xtable::xtable(
	  df,
	  caption = caption,
	  align = align
	  )

	print(
	  latextable,
	  hline.after = c(-1, 0),
	  file = filn,
	  include.rownames = FALSE,
	  ...
	  )

	if (process_citations){
	  # Read the .tex file again for line-by-line corrections
	  lines <- readLines(filn)

	  # Apply the processing function to each line
	  modified_lines <- process_cite_lines(lines)

	  # Write the modified lines to a new .tex file
	  writeLines(
	    modified_lines,
	    filn
	  )
	}

  return(filn)
}

