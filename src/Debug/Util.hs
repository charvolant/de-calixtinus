module Debug.Util (
    traceTagShowId
) where
  
import Debug.Trace

-- Trace a value with a tage showing what it is
traceTagShowId :: (Show a) => String -> a -> a
traceTagShowId tag a = trace (tag ++ " " ++ show a) a

