package analyser

import "fmt"

type stringset map[string]bool

func (set stringset) slice() (slc []string) {
	for k := range set {
		slc = append(slc, k)
	}

	return
}

func (set stringset) add(items ...string) stringset {
	for _, item := range items {
		set[item] = true
	}

	return set
}

func (set stringset) merge(sets ...stringset) stringset {
	for _, s := range sets {
		set.add(s.slice()...)
	}

	return set
}

func (set stringset) delete(items ...string) stringset {
	for _, item := range items {
		delete(set, item)
	}

	return set
}

func (set stringset) String() string {
	return fmt.Sprintf("%v", set.slice())
}

func (set stringset) contains(item string) bool {
	return set[item]
}
