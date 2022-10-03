# Naming

need to determine a naming convention that will work across sessions
require: useful function names (concise, indicative of use, etc), that ALSO
   enable quick lookup by session
desire: stable against future revisions to course schedule (e.g. reordering
   sessions); ideally prefix based to enable `?MTM::SESSION_` to suggest

Preliminary solution: short, "nickname" prefix by session
Using `network_` for network session functions

# Tutorials vs scripts

for the scripts vs learnr versions of package materials, determine some way to have write-once approach? might not be appropriate, as likely some distinctions between what material is most effective in what format?

# Scripts w/ and w/out solutions

definitely for the solutions vs scripts versions:
 - define a "solution block" identifier
 - create scripted solution to read all the solution files, replace the solution block with a TODO block, and write the result to the corresponding not-solution file
 
# Tutorials

define a "first section" block that will remind people about the interface across sessions. Maybe this helpful: https://stackoverflow.com/questions/61459363/including-external-markdown-file-covering-inline-code-through-includemarkdown

