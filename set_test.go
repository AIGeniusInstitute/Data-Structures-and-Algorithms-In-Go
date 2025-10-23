package Data_Structures_and_Algorithms_In_Go

import "testing"

func TestSet(t *testing.T) {
	set := NewSet()
	set.Add(0)
	set.Add(1)
	set.Add(2)
	set.Add(2)
	True(t, set.Contains(1))
	Equal(t, 3, set.Len())
	set.Remove(2)
	Equal(t, 2, set.Len())
	False(t, set.Contains(2))
}
