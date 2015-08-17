
tabla1 <- data.frame(year, weeks)
eptabla$Año <- factor(eptabla$Año)

A = data.frame(a = c(1,5,3))
B = data.frame(a_ = c(1,2), otra = c("josue","jacky"))
merged.data.all <- merge(x = A, y = B, by.x="a", by.y="a_", all=TRUE)
merged.data.all