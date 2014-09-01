####
# If you're concatenating strings in a for loop, you should probably be using this.
# Provides a nice interface to build a string, similar to java's StringBuilder class.
# Most of these are just aliases, just relabelled for convenience.
# example:
#     sb = StringBuilder()
#     for i = 1:10
#         append(sb,i)
#     end
#     my_string = get_string(sb)    ->    "12345678910"
####
StringBuilder = IOBuffer

append(sb::StringBuilder,x) = print(sb,x)

function get_string(sb::StringBuilder)
  sb.writable = false
  data = takebuf_string(sb)
  sb.writable = true
end
