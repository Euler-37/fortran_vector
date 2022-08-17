 program check
   use fortran_vector
   implicit none
   type(vector_int)::a
   integer,allocatable::x(:)
   integer::i
   integer,parameter::n=100000
   integer(8)::tic,toc,rate
   !----------------
   call system_clock(tic,rate)
   call a%init()
   do i=1,n
      call a%append(i)
   end do
   call a%cut()
   call system_clock(toc,rate)
   write(*,*)"vector int",real(toc-tic,8)/rate,a%get(n)
   !----------------
   call system_clock(tic,rate)
   allocate(x(0))
   do i=1,n
      x=[x,i]
   end do
   call system_clock(toc,rate)
   write(*,*)"append",real(toc-tic,8)/rate,x(n)
   !----------------
   deallocate(x)
   call system_clock(tic,rate)
   allocate(x(n))
   do i=1,n
      x(i)=i
   end do
   call system_clock(toc,rate)
   write(*,*)"array",real(toc-tic,8)/rate,x(n)
end program check
