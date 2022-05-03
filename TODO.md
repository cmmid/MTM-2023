need to determine a naming convention that will work across sessions
require: useful function names (concise, indicative of use, etc), that ALSO
   enable quick lookup by session
desire: stable against future revisions to course schedule (e.g. reordering
   sessions); ideally prefix based to enable `?MTM::SESSION_` to suggest

Preliminary solution: short, "nickname" prefix by session
Using `network_` for network session functions

learnr is a somewhat heavy dependency; should make it optional at package install, for versions of the course that don't care about using it -- i.e. just want to use the scripts version of the materials?

for the scripts vs learnr versions of package materials, determine some way to have write-once approach? might not be appropriate, as likely some distinctions between what material is most effective in what format?
