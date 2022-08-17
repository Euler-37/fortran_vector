program main
   use fortran_vector
   implicit none
   type(vector_int)::a
   integer::i
   call a%init()
   call a%append([1,1,2,2,333,33,33,44,55,66,77,88,9,10,10])
   call a%write()
   call a%unique()
   call a%write()
end program main
