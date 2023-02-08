package support

import "testing"

func genString(l int) string {
	str := make([]byte, 0, l)
	for i := 0; i < l; i++ {
		str = append(str, byte(i%10+'0'))
	}
	return string(str)
}

func TestFastString(t *testing.T) {

	t.Run("long", func(t *testing.T) {
		exp := genString(32145)
		got := Prelude_types_fastPack(Prelude_types_fastUnpack(exp))

		if exp != got {
			t.Errorf("expected %s\nbut got %s", exp, got)
		}
	})

	t.Run("empty", func(t *testing.T) {
		exp := ""
		got := Prelude_types_fastPack(Prelude_types_fastUnpack(exp))

		if exp != got {
			t.Errorf("expected %s\nbut got %s", exp, got)
		}
	})
}
