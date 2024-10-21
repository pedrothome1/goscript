i := -1
max := 10

for true {
    i++
    if i > max {
        break
    }
    if i % 2 == 1 {
        print "odd"
        continue
    }
    print "even"
}
