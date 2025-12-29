program d1
    use safe
    ! importante pra dizer q as vars vão ser todas declaradas explicitamente
    implicit none 
    
    integer :: tam                          ! tamanho da entrada
    character(len = 128) :: buf             ! buffer
    integer :: ios                          ! controle leitor buffer
    integer :: rep(10)                      ! status e vetor do report
    
    integer :: input, stat                  ! variaveis controle p/ abrir os arq
    character(len=512) :: msg
    
    integer :: i , j                        ! iteradores
    logical :: s                            ! controle safe
    integer :: safes                        ! numero de safes (resposta)
    
    
    ! abrindo a lista
    open(newunit=input, file="../input", status="old", action="read", iostat=stat, iomsg=msg)
    if (stat /= 0) then
        print *, trim(msg)
        error stop
    end if
    
    safes = 0
    
    ! lendo input
    do while (.true.)
        read(input, '(a)', END = 200) buf
        
        rep = -1
        
        read(buf, *, iostat = ios) rep
        
        s = isSafe(rep)
            
        if (s) then
            safes = safes + 1
        end if
        
    end do
    
    200 CONTINUE
    
    close(input)
    
    print *, safes
    
end program d1
