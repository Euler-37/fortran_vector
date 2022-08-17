# fortran_vector
fortran vector_int, the type defined as
``` fortran
type::vector_int
    integer,allocatable::x_(:)
    integer::num_
    integer::capacity_
end type vector_int
```

## init
set dim
``` fortran
type(vector_int)::a
call a%init()
! or
!call a%init(22333)
```

## append
append elements, value or array

``` fortran
call a%append(10)
call a%append([1,2,3,4])
```
## size
return size of vector

``` fortran
  write(*,*)size(a)
```

## pop
pop last element
``` fortran
  write(*,*)a%pop()
```

## remove
remove all `val`
``` fortran
  !! a=[1,2,3,3,4]
  call a%remove(3)
  !! a=[1,2,4]
```
## delete
delete i-th `val`
``` fortran
  !! a=[1,3,2,4,9]
  call a%remove(2)
  !! a=[1,2,4,9]
```
## unique
remove duplicate elements
``` fortran
  !! a=[1,1,3,2,2,4,4,9,10]
  call a%unique()
  !! a=[1,3,2,4,9,10]
```
## sort
sort vector
``` fortran
  !! a=[1,1,3,2,2,4,4,9,10]
  call a%sort()
  !! a=[1,1,2,2,3,4,4,9,10]
```

## cut
cut Extra memory
``` fortran
  ! a%capacity_==a%num_
  call a%cut()
```


