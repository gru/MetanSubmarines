namespace Metan.Core

open System

type BotState =
    | SearchCrate
    | MoveTo of Position
    | Idle of Time

module AI =
    let private findById id game =
        game.vehicles |> List.find (fun v -> v.id = id)
    
    let private findClosestCrate cs hb =
        cs |> List.filter (fun c -> HitBox.intersect c.hitBox hb)
           |> List.tryHead
        
    let private contains pos = function
        | Projection sl ->
            sl |> List.map Reflection.getPos |> List.contains pos
        
    let shrink (w, h) =
        (w - 1, h - 1)
        
    let searchCrate rnd game id =
        let me = findById id game
        let crate = me.hitBox
                    |> HitBox.expand 10
                    |> HitBox.crop game.size
                    |> findClosestCrate game.crates
        match crate with
        | Some c -> MoveTo(HitBox.topLeft c.hitBox), []
        | None -> MoveTo(Position.getRandom rnd game.size), [] 
        
    let move game id pos =
        let me = findById id game
        let pr = Projection.project me.hitBox me.shape
        if pr |> contains pos then
            Idle (game.time + 20u), []
        else
            let tl = Projection.topLeft pr
            let lx = (fst tl) - (fst pos)
            let ly = (snd tl) - (snd pos)
            if abs lx > abs ly then
                let dir =
                    if lx > 0
                    then Left
                    else Right
                (MoveTo pos), [AreaCommand(Move(me.id, dir))]
            else
                let dir =
                    if ly < 0
                    then Up
                    else Down
                (MoveTo pos), [AreaCommand(Move(me.id, dir))]
        
    let idle game till =
        if game.time = till
        then SearchCrate, []
        else Idle till, []