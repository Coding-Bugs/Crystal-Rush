Public Class Pos

    Public Property X As Integer = -1
    Public Property Y As Integer = -1

    Public Sub New(ByVal x, ByVal y)
        Me.X = x
        Me.Y = y
    End Sub

    Public Function distance(ByVal targetX As Integer, ByVal targetY As Integer)
        Return Math.Abs(Me.X - targetX) + Math.Abs(Me.Y - targetY)
    End Function

End Class


Public Class Cell
    Inherits Pos

    Public Property Ore As String
    Public Property Hole As Integer
    Public Property OreRemaining As Integer = -1

    Public Sub New(ByVal x As Integer, ByVal y As Integer, Optional ByVal ore As String = "", Optional ByVal hole As Integer = 0)
        MyBase.New(x, y)
        Me.Ore = ore
        Me.Hole = hole
    End Sub

    Public Sub Update(ByVal ore As String, ByVal hole As Integer)
        Me.Ore = ore
        Me.Hole = hole
        If Me.Ore <> "?" And Me.OreRemaining = -1 Then
            Me.OreRemaining = Asc(Me.Ore) - Asc("0")
        End If

    End Sub

    Public Function HasOreRemaining()
        If Me.OreRemaining > 0 Then
            Return True
        End If
        Return False
    End Function

End Class


Public Class Grid
    Public Property Cells As Cell()
    Public Property Width
    Public Property Height

    Public Sub New(Optional ByVal width As Integer = 29, Optional ByVal height As Integer = 14)
        Me.Width = width
        Me.Height = height
        ReDim Me.Cells((width + 1) * (height + 1))

        For y As Integer = 0 To height
            For x As Integer = 0 To width
                Cells((y * width) + x) = New Cell(x, y)
            Next
        Next

    End Sub

    Public Function GetCell(ByVal x As Integer, ByVal y As Integer)
        If (Me.Width >= x Or x >= 0) Or (Me.Height >= y Or y >= 0) Then
            Return Me.Cells(y * Me.Width + x)
        End If
        Return Nothing
    End Function

    Public Sub PrintCells()
        For j As Integer = 0 To Me.Height
            For i As Integer = 0 To Me.Width
                Console.Error.Write(Me.Cells(j * Me.Height + i).Ore & Me.Cells(j * Me.Height + i).Hole)
                If i < Me.Width Then
                    Console.Error.Write(" ")
                End If
            Next
            Console.Error.WriteLine()
        Next
    End Sub
End Class


Public Class Entity
    Inherits Pos
    Public Property Id As Integer = -1
    Public Property EntityType As Integer = -1

    Public Sub New(ByVal x As Integer, ByVal y As Integer, ByVal id As Integer, ByVal type As Integer)
        MyBase.New(x, y)
        Me.Id = id
        Me.EntityType = type
    End Sub

End Class


Public Class Robot
    Inherits Entity

    Public Property Verbose As Boolean = False
    Public Property Dug As Boolean = False
    Public Property Inv As Integer = -1
    Public Property Task As String = ""
    Public Property TaskQueue As Queue = New Queue()

    Public Sub New(ByVal x As Integer, ByVal y As Integer, ByVal id As Integer, ByVal type As Integer, ByVal inv As Integer)
        MyBase.New(x, y, id, type)
        Me.Inv = inv
    End Sub

    Public Sub Update(ByVal x As Integer, ByVal y As Integer, ByVal inv As Integer)
        Me.X = x
        Me.Y = y
        Me.Inv = inv
    End Sub

    Public Sub GotOre()
        If Me.Verbose = True Then
            Console.Error.WriteLine("Robot #" & Me.Id & " has ore, is overwriting orders and returning to base")
        End If

        ' override current task
        Me.Task = "MOVE 0 " & Me.Y

        ' clear queue
        ClearQueue()
    End Sub

    Public Sub ClearQueue()
        ' clear the queue 
        While Me.TaskQueue.Count > 0
            Me.TaskQueue.Dequeue()
        End While
    End Sub


    Public Sub AssignTask(ByVal command As String)
        Me.Task = command
    End Sub


    Public Sub AddTask(ByVal command As String)
        Me.TaskQueue.Enqueue(command)
    End Sub


    Public Function GetNextTask()
        If Me.TaskQueue.Count = 0 Then
            Return ""
        End If
        Return Me.TaskQueue.Dequeue()
    End Function


    Public Sub Process()

        Console.Error.WriteLine("Processing Task {0}", Me.Task)

        If Me.Task = "" Then
            AssignTask(GetNextTask())
            Return
        End If

        Dim command As String() = Me.Task.Split(" ")

        If command(0) = "REQUEST" Then
            ' The REQUEST command is only completed if bot receives item
            If Inv = 2 Or Inv = 3 Then
                AssignTask(GetNextTask())
            End If
        ElseIf command(0) = "DIG" Then
            ' The dig command is only completed if I have ore, or I have already tried to dig in my target already 
            If Inv = 4 Then
                GotOre()
            ElseIf Inv = -1 And ((Me.X = command(1) And (Me.Y <= command(2) + 1 And Me.Y >= command(2) - 1)) Or
                                 (Me.Y = command(2) And (Me.X <= command(1) + 1 And Me.X >= command(1) - 1))) Then
                If Dug = True Then
                    Dug = False
                    AssignTask(GetNextTask())
                Else
                    Dug = True
                End If
            Else
                Dug = False
            End If
        ElseIf command(0) = "MOVE" Then
            If (Me.X = command(1) And (Me.Y <= command(2) + 1 And Me.Y >= command(2) - 1)) Or
               (Me.Y = command(2) And (Me.X <= command(1) + 1 And Me.X >= command(1) - 1)) Then
                AssignTask(GetNextTask())
            End If
        ElseIf command(0) = "WAIT" Then
            AssignTask(GetNextTask())
        Else
            AssignTask(GetNextTask())
            ClearQueue()
        End If
    End Sub


    Public Sub Execute()
        Process()
        Console.Error.WriteLine("Executing Order: {0}", Me.Task)
        If Me.Task = "" Then
            Console.WriteLine("WAIT")
        Else
            Console.WriteLine(Me.Task)
        End If
    End Sub



End Class


Class Game
    Public Property Grid As Grid

    Public Property MyScore As Integer
    Public Property EnemyScore As Integer
    Public Property RadarCooldown As Integer
    Public Property TrapCooldown As Integer

    Public Property Verbose As Boolean
    Public Property RadarReady As Boolean

    Public Property Radars As List(Of Entity)
    Public Property Traps As List(Of Entity)
    Public Property MyRobots As Dictionary(Of Integer, Robot)
    Public Property EnemyRobots As Dictionary(Of Integer, Robot)

    Public Sub New(Optional ByVal width As Integer = 30, Optional ByVal height As Integer = 15)
        Me.Grid = New Grid(width - 1, height - 1)

        Me.MyScore = 0
        Me.EnemyScore = 0
        Me.RadarCooldown = 0
        Me.TrapCooldown = 0

        Me.Verbose = False
        Me.RadarReady = True

        Me.Radars = New List(Of Entity)
        Me.Traps = New List(Of Entity)
        Me.MyRobots = New Dictionary(Of Integer, Robot)
        Me.EnemyRobots = New Dictionary(Of Integer, Robot)
    End Sub


    Public Sub PrintMyRobots()
        For Each bot As Robot In Me.MyRobots.Values()
            Console.Error.WriteLine("Ally Id:{0}, X: {1}, Y: {2}, Inv:{3}", bot.Id, bot.X, bot.Y, bot.Inv)
        Next
        Console.Error.WriteLine()
    End Sub


    Public Sub PrintEnemyRobots()
        For Each bot As Robot In Me.EnemyRobots.Values()
            Console.Error.WriteLine("Enemy Id:{0}, X: {1}, Y: {2}, Inv:{3}", bot.Id, bot.X, bot.Y, bot.Inv)
        Next
        Console.Error.WriteLine()
    End Sub


    Public Sub PrintRadars()
        For Each ent As Entity In Me.Radars
            Console.Error.WriteLine("Radar Id:{0}, X: {1}, Y: {2}", ent.Id, ent.X, ent.Y)
        Next
        Console.Error.WriteLine()
    End Sub


    Public Sub PrintTraps()
        For Each ent As Entity In Me.Traps
            Console.Error.WriteLine("Trap Id:{0}, X: {1}, Y: {2}", ent.Id, ent.X, ent.Y)
        Next
        Console.Error.WriteLine()
    End Sub
End Class


Module Player
    ' Deliver more ore to hq (left side of the map) than your opponent. Use radars to find ore but beware of traps!
    Public Function GetRand(ByVal Upper As Integer, ByVal Lower As Integer) As Integer
        ' by making Generator static, we preserve the same instance
        ' (i.e., do not create new instances with the same seed over and over) 
        ' between calls
        Static Generator As New System.Random()
        Return Generator.Next(Lower, Upper)
    End Function


    'Function walkRadars(ByVal radars As Entity())

    'End Function

    Sub Main()
        Dim inputs As String() = Console.ReadLine().Split(" ")

        Dim game As New Game(inputs(0), inputs(1))
        game.Verbose = True
        If game.Verbose = True Then
            Console.Error.WriteLine("Width: " & game.Grid.Width)
            Console.Error.WriteLine("Height: " & game.Grid.Height)
            Console.Error.WriteLine()
        End If


        ' game loop
        While True
            inputs = Console.ReadLine().Split(" ")


            ' Update Scores
            game.MyScore = inputs(0)
            game.EnemyScore = inputs(1)
            If game.Verbose = True Then
                Console.Error.WriteLine("MyScore: " & game.MyScore)
                Console.Error.WriteLine("EnemyScore: " & game.EnemyScore)
                Console.Error.WriteLine()
            End If


            ' Update Cells
            For j As Integer = 0 To game.Grid.Height
                inputs = Console.ReadLine().Split(" ")
                For i As Integer = 0 To game.Grid.Width
                    Dim ore As String = inputs(2 * i) ' amount of ore or "?" if unknown
                    Dim hole As Integer = inputs(2 * i + 1) ' 1 if cell has a hole

                    game.Grid.Cells(j * game.Grid.Height + i).Update(ore, hole)
                Next
            Next
            ' Print updated board
            If game.Verbose = True Then
                Console.Error.WriteLine("Board:")
                game.Grid.PrintCells()
                Console.Error.WriteLine()
            End If


            Dim entityCount As Integer ' number of entities visible to you
            inputs = Console.ReadLine().Split(" ")
            entityCount = inputs(0)


            ' Update Cooldowns
            game.RadarCooldown = inputs(1) ' turns left until a new radar can be requested
            game.TrapCooldown = inputs(2) ' turns left until a new trap can be requested
            If game.Verbose = True Then
                Console.Error.WriteLine("Radar Cooldown: " & game.RadarCooldown)
                Console.Error.WriteLine("Trap Cooldown: " & game.TrapCooldown)
                Console.Error.WriteLine()
            End If

            If game.RadarCooldown > 0 Then
                game.RadarReady = True
            End If


            ' Clear last turns data
            game.Radars.Clear()
            game.Traps.Clear()

            ' Update Entities
            For i As Integer = 0 To entityCount - 1
                Dim entityId As Integer ' unique id of the entity
                Dim entityType As Integer ' 0 for your robot, 1 for other robot, 2 for radar, 3 for trap
                Dim x As Integer
                Dim y As Integer ' position of the entity
                Dim item As Integer ' if this entity is a robot, the item it is carrying (-1 for NONE, 2 for RADAR, 3 for TRAP, 4 for ORE)
                inputs = Console.ReadLine().Split(" ")
                entityId = inputs(0)
                entityType = inputs(1)
                x = inputs(2)
                y = inputs(3)
                item = inputs(4)

                If entityType = 0 Then
                    If game.MyRobots.ContainsKey(entityId) Then
                        game.MyRobots(entityId).Update(x, y, item)
                    Else
                        game.MyRobots.Add(entityId, New Robot(x, y, entityId, entityType, item))
                    End If
                ElseIf entityType = 1 Then
                    If game.EnemyRobots.ContainsKey(entityId) Then
                        game.EnemyRobots(entityId).Update(x, y, item)
                    Else
                        game.EnemyRobots.Add(entityId, New Robot(x, y, entityId, entityType, item))
                    End If
                ElseIf entityType = 2 Then
                    game.Radars.Add(New Entity(x, y, entityId, entityType))
                Else
                    game.Traps.Add(New Entity(x, y, entityId, entityType))
                End If
            Next
            ' Print Updated Entities and Robots
            If game.Verbose = True Then
                game.PrintMyRobots()
                game.PrintEnemyRobots()
                game.PrintRadars()
                game.PrintTraps()
            End If

            ' Command Space
            For Each bot As Robot In game.MyRobots.Values()
                ' Print Bot's info
                If game.Verbose = True Then
                    Console.Error.WriteLine("Bot#{0} X: {1}, Y: {2}, Inv: {3}, Task: {4}", bot.Id, bot.X, bot.Y, bot.Inv, bot.Task)
                End If


                Dim x As Integer = 0
                Dim y As Integer = 0


                ' Check for primary derivative  
                If bot.Task = "" And bot.TaskQueue.Count = 0 Then
                    If bot.Inv = 4 Then
                        bot.GotOre()
                    ElseIf game.RadarCooldown = 0 And game.RadarReady = True Then
                        ' Place first radar at 1/4 horizontally and 1/2 vertically away from the HQ
                        If game.Radars.Count = 0 Then
                            x = Int(game.Grid.Width / 4)
                            y = Int(game.Grid.Height / 2)
                        Else
                            x = GetRand(game.Grid.Width - 4, 4)
                            y = GetRand(game.Grid.Height - 4, 4)
                        End If

                        If game.Verbose = True Then
                            Console.Error.WriteLine("Ordering Bot#{0} to put RADAR at ({1}, {2})", bot.Id, x, y)
                        End If

                        bot.AssignTask("REQUEST RADAR")
                        bot.AddTask("DIG " & x.ToString() & " " & y.ToString())
                        bot.AddTask("WAIT")

                        game.RadarReady = False

                    Else
                        If game.Verbose = True Then
                            Console.Error.WriteLine("Ordering Bot#{0} to MOVE to ({1}, {2})", bot.Id, bot.X, bot.Y)
                        End If

                        bot.Task = "MOVE " & GetRand(Int(game.Grid.Width * 2 / 3), Int(game.Grid.Width * 1 / 4)) & " " & GetRand(Int(game.Grid.Height * 2 / 3), Int(game.Grid.Height * 1 / 4))
                    End If
                End If
                bot.Execute()
                Console.Error.WriteLine()
            Next

        End While
    End Sub
End Module