module CUDAFortranKernels
    use, intrinsic :: ISO_Fortran_env
    use BabelStreamTypes

    contains
        attributes(global) subroutine do_copy(n,A,C)
            implicit none
            integer(kind=StreamIntKind), intent(in), value :: n
            real(kind=REAL64), intent(in)  :: A(n)
            real(kind=REAL64), intent(out) :: C(n)
            integer(kind=StreamIntKind) :: i
            i = blockDim%x * (blockIdx%x - 1) + threadIdx%x
            if (i <= N) then
               C(i) = A(i)
            endif
        end subroutine do_copy

        attributes(global) subroutine do_add(n,A,B,C)
            implicit none
            integer(kind=StreamIntKind), intent(in), value :: n
            real(kind=REAL64), intent(in)  :: A(n), B(n)
            real(kind=REAL64), intent(out) :: C(n)
            integer(kind=StreamIntKind) :: i
            i = blockDim%x * (blockIdx%x - 1) + threadIdx%x
            if (i <= N) then
               C(i) = A(i) + B(i)
            endif
        end subroutine do_add

        attributes(global) subroutine do_mul(n,scalar,B,C)
            implicit none
            integer(kind=StreamIntKind), intent(in), value :: n
            real(kind=REAL64), intent(in), value :: scalar
            real(kind=REAL64), intent(out) :: B(n)
            real(kind=REAL64), intent(in)  :: C(n)
            integer(kind=StreamIntKind) :: i
            i = blockDim%x * (blockIdx%x - 1) + threadIdx%x
            if (i <= N) then
               B(i) = scalar * C(i)
            endif
        end subroutine do_mul

        attributes(global) subroutine do_triad(n,scalar,A,B,C)
            implicit none
            integer(kind=StreamIntKind), intent(in), value :: n
            real(kind=REAL64), intent(in), value :: scalar
            real(kind=REAL64), intent(out) :: A(n)
            real(kind=REAL64), intent(in)  :: B(n), C(n)
            integer(kind=StreamIntKind) :: i
            i = blockDim%x * (blockIdx%x - 1) + threadIdx%x
            if (i <= N) then
               A(i) = B(i) + scalar * C(i)
            endif
        end subroutine do_triad

        attributes(global) subroutine do_nstream(n,scalar,A,B,C)
            implicit none
            integer(kind=StreamIntKind), intent(in), value :: n
            real(kind=REAL64), intent(in), value :: scalar
            real(kind=REAL64), intent(inout) :: A(n)
            real(kind=REAL64), intent(in)    :: B(n), C(n)
            integer(kind=StreamIntKind) :: i
            i = blockDim%x * (blockIdx%x - 1) + threadIdx%x
            if (i <= N) then
               A(i) = A(i) + B(i) + scalar * C(i)
            endif
        end subroutine do_nstream

        attributes(global) subroutine do_dot(n,A,B,r)
            implicit none
            integer(kind=StreamIntKind), intent(in), value :: n
            real(kind=REAL64), intent(in) :: A(n), B(n) 
            real(kind=REAL64), intent(out) :: r
            integer(kind=StreamIntKind) :: i
            r = real(0,kind=REAL64)
            !$cuf kernel do <<< *, * >>>
            do i=1,N
               r = r + A(i) * B(i)
            end do
        end subroutine do_dot

end module CUDAFortranKernels

module CUDAStream
    use, intrinsic :: ISO_Fortran_env
    use BabelStreamTypes
    use cudafor, only: dim3

    implicit none

    character(len=4), parameter :: implementation_name = "CUDA"

    integer(kind=StreamIntKind) :: N

    real(kind=REAL64), allocatable, managed :: A(:), B(:), C(:)

    type(dim3) :: grid, tblock

    contains

        subroutine list_devices()
            use cudafor
            implicit none
            integer :: num, err
            err = cudaGetDeviceCount(num)
            if (err.ne.0) then
              write(*,'(a12,i4)') "CUDA error: ",err
              stop
            else if (num.eq.0) then
              write(*,'(a17)') "No devices found."
            else
              write(*,'(a10,i1,a8)') "There are ",num," devices."
            end if
        end subroutine list_devices

        subroutine set_device(dev)
            use cudafor
            implicit none
            integer, intent(in) :: dev
            integer :: num, err
            err = cudaGetDeviceCount(num)
            if (err.ne.0) then
              write(*,'(a)') "cudaGetDeviceCount failed"
              write(*,'(a)') cudaGetErrorString(err)
              stop
            else if (num.eq.0) then
              write(*,'(a17)') "No devices found."
              stop
            else if (dev.ge.num) then
              write(*,'(a21)') "Invalid device index."
              stop
            else
              err = cudaSetDevice(dev)
              if (err.ne.0) then
                write(*,'(a)') "cudaSetDevice failed"
                write(*,'(a)') cudaGetErrorString(err)
                stop
              end if
            end if
        end subroutine set_device

        subroutine alloc(array_size)
            implicit none
            integer(kind=StreamIntKind) :: array_size
            integer :: err
            N = array_size
            allocate( A(1:N), B(1:N), C(1:N), stat=err)
            if (err .ne. 0) then
              write(*,'(a20,i3)') 'allocate returned ',err
              stop 1
            endif
            ! move to separate subroutine later
            tblock = dim3(128,1,1)
            grid = dim3(ceiling(real(N)/tblock%x),1,1)
        end subroutine alloc

        subroutine dealloc()
            implicit none
            integer :: err
            deallocate( A, B, C, stat=err)
            if (err .ne. 0) then
              write(*,'(a20,i3)') 'deallocate returned ',err
              stop 1
            endif
        end subroutine dealloc

        subroutine init_arrays(initA, initB, initC)
            implicit none
            real(kind=REAL64), intent(in) :: initA, initB, initC
            integer(kind=StreamIntKind) :: i
            A = initA
            B = initB
            C = initC
        end subroutine init_arrays

        subroutine read_arrays(h_A, h_B, h_C)
            implicit none
            real(kind=REAL64), intent(inout) :: h_A(:), h_B(:), h_C(:)
            integer(kind=StreamIntKind) :: i
            h_A = A
            h_B = B
            h_C = C
        end subroutine read_arrays

        subroutine copy()
            use CUDAFortranKernels, only: do_copy
            implicit none
            call do_copy<<<grid, tblock>>>(N, A, C)
        end subroutine copy

        subroutine add()
            use CUDAFortranKernels, only: do_add
            implicit none
            call do_add<<<grid, tblock>>>(N, A, B, C)
        end subroutine add

        subroutine mul(startScalar)
            use CUDAFortranKernels, only: do_mul
            implicit none
            real(kind=REAL64), intent(in) :: startScalar
            real(kind=REAL64) :: scalar
            scalar = startScalar
            call do_mul<<<grid, tblock>>>(N, scalar, B, C)
        end subroutine mul

        subroutine triad(startScalar)
            use CUDAFortranKernels, only: do_triad
            implicit none
            real(kind=REAL64), intent(in) :: startScalar
            real(kind=REAL64) :: scalar
            scalar = startScalar
            call do_triad<<<grid, tblock>>>(N, scalar, A, B, C)
        end subroutine triad

        subroutine nstream(startScalar)
            use CUDAFortranKernels, only: do_nstream
            implicit none
            real(kind=REAL64), intent(in) :: startScalar
            real(kind=REAL64) :: scalar
            scalar = startScalar
            call do_nstream<<<grid, tblock>>>(N, scalar, A, B, C)
        end subroutine nstream

        function dot() result(r)
            !use CUDAFortranKernels, only: do_dot
            implicit none
            real(kind=REAL64) :: r
            !integer(kind=StreamIntKind) :: i
            !call do_dot<<<grid, tblock>>>(N, B, C, r)
            !r = real(0,kind=REAL64)
            !$acc parallel loop reduction(+:r)
            !do i=1,N
            !   r = r + A(i) * B(i)
            !end do
            !r = dot_product(A,B)
        end function dot

end module CUDAStream
