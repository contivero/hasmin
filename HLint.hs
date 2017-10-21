import "hint" HLint.Default
import "hint" HLint.Dollar
import "hint" HLint.Builtin.All

-- General
warn = x *> pure y ==> x $> y
warn = pure x <* y ==> x <$ y
warn = x >> return y ==> x $> y
warn = return x << y ==> x <$ y
warn = return x ==> pure x -- Every monad is an applicative functor
warn = m >>= pure . f ==> f <$> m
warn = pure . f =<< m ==> f <$> m

-- Hasmin specific
warn = skipComments *> x <* skipComments ==> lexeme x
