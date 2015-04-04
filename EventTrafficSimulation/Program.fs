// Learn more about F# at http://fsharp.net
// See the 'F# Tutorial' project for more help.
open System

type PCU = 
    | Traffic of float
    | Zero

type Tick =
    | Timestep of int

type packet = {
    Flow : PCU;
    Origin : Guid;
    ArriveAt : Tick;
    }

type Msg = 
    | Tick of Tick
    | Packet of packet

type Link = {
    Length : int;
}

type Junction = {
    CycleTime : int;
}

let BuildLinks() = 
    [for i in 1 .. 10 -> { Length = 200 }]

let BuildJunctions() =
    [for i in 1 .. 10 -> { CycleTime = 120 }]

let BuildNetwork() =
    (BuildLinks() , BuildJunctions())
    
let simulation = MailboxProcessor.Start(fun agent ->
    let rec loop network count = async {
        let! msg = agent.Receive()
        match msg with
        | Tick t ->
            // todo: release all held until this tick
            match t with
            | Timestep t' -> printfn "%i" t'
        | Packet p ->
            let receivers = [1,2,3]
            for i in receivers do agent.Post(Packet {
                Flow = match p.Flow with | Traffic f -> Traffic (f/3.0) | Zero -> Zero; 
                Origin = Guid.NewGuid(); 
                ArriveAt = Timestep 5
                })
        }
    loop (BuildNetwork()) 0
)
//    let simulation = MailboxProcessor.Start(fun agent ->
//        let rec loop network count = async {
//            let! msg = agent.Receive()
//            match msg with
//            | Tick t ->
//                // todo: release all held until this tick
//                match t with
//                | Timestep t' -> printfn "%i" t'
//            | Packet p ->
//                let receivers = [1,2,3]
//                for i in receivers do agent.Post(Packet {
//                    Flow = match p.Flow with | Traffic f -> Traffic (f/3.0) | Zero -> Zero; 
//                    Origin = Guid.NewGuid(); 
//                    ArriveAt = Timestep 5
//                    })
//            }
//        loop (BuildNetwork()) 0
//    )
type msg = 
        | CreateAgent
        | DoStuff of string
        | DoStuffResponse of string

[<EntryPoint>]
let main argv = 

    let pool = MailboxProcessor.Start(fun master ->
        let rec messageloop agents = async {
            let! msg = master.Receive()
            match msg with
            | CreateAgent ->
                printfn "Received CreateAgent"
                let newagent = MailboxProcessor.Start(fun agent ->
                    let rec forever = async {
                        let! mymsg = agent.Receive();
                        match mymsg with
                        | DoStuff t ->
                            printfn "Responding to DoStuff"
                            master.Post(DoStuffResponse "response")
                            |> ignore
                        | _ -> ()
                        return! forever
                    }
                    forever
                )
                return! messageloop(newagent :: agents)
            | DoStuffResponse t ->
                printfn "Received response"
                return! messageloop(agents)
            | _ -> 
                printfn "Relaying message"
                for agent in agents do agent.Post msg
                return! messageloop(agents)
        }
        messageloop []
    )

    pool.Post(CreateAgent)
    pool.Post(CreateAgent)
    pool.Post(DoStuff "hello")
                  
    System.Console.ReadKey() |> ignore
    printfn "%A" argv
    0 // return an integer exit code