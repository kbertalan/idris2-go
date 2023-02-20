module FFI

%foreign """
  go:
  import "fmt"

  func (s, w any) any {
    fmt.Println(s.(string))
    return nil
  }
  """
fmtPrintln : String -> PrimIO ()

%foreign """
  go:
  import (
    "fmt"
    %support%
  )

  func (s, w any) any {
    fmt.Println(s.(string), "with multiple imports")
    return support.Constructor(0)
  }
  """
fmtPrintlnAndSomethingElse : String -> PrimIO ()

main : IO ()
main = do
  primIO $ fmtPrintln "This is printed through FFI"
  primIO $ fmtPrintlnAndSomethingElse "Second"

