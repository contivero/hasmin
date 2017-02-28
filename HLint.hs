import "hint" HLint.Default
import "hint" HLint.Dollar
import "hint" HLint.Builtin.All

warn = x *> pure y ==> x $> y
warn = pure x <* y ==> x <$ y
warn = x >> return y ==> x $> y
warn = return x << y ==> x <$ y
warn = skipComments *> x <* skipComments ==> lexeme x
