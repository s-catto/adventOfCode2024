program p1
    
    use modelos
    
    implicit none
    
    character(len=8), parameter :: inputName = "../input"
    !character(len=10), parameter :: inputName = "../inputex"
    
    integer :: input, stat                  ! variaveis controle p/ abrir os arq
    character(len=512) :: msg
    
    character :: a                          ! caractere a ser lido do input
    integer :: i                            ! iteradores
    logical :: d0                           ! do (1) ou dont (0)
    integer :: mult                         ! mult atual
    
    integer :: multTudo                     ! soma total dos mults (resposta)
    
    ! abrindo o input
    open(newunit=input, file= inputName, status="old", access="stream", action="read", iostat=stat, iomsg=msg)
    if (stat /= 0) then
        print *, trim(msg)
        error stop
    end if
    
    i = 1
    multTudo = 0
    d0 = .true.
    mult = 0
    
    ! p/ cada caractere, testa se é o comeco de todos os modelos
    ! se sim, entra na funcao que termina de identificar
    ! se nao identificar, consome e retorna ao loop principal 
    do while (.true.)
    
        read(input, POS=i, END = 200) a
        
        ! consome os caracteres e calcula quando acha mul
        mult = multId(inputName, i)
        
        ! consome os caracteres e identifica do e dont
        select case (doId(inputName, i))
        
            case (1)
                d0 = .false.
            case (2)
                d0 = .true.
                
            case Default
        
        end select
        
        ! se esta do, incrementa multTudo
        if (d0) then
            multTudo = multTudo + mult
        end if
        
        i = i + 1
        
    end do
    
    200 CONTINUE
    
    print *, multTudo
    
    close(input)
    
end program p1
