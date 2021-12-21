namespace Puzzles
open System.IO
open System.Reflection

module inputs =
    type marker = interface end

    let GetResourceStream name =
        let assembly = Assembly.GetAssembly(typeof<marker>)
        assembly.GetManifestResourceStream("Puzzles.inputs." + name)
        
    let readAllLines name =
         seq {
            use stream = GetResourceStream(name)
            use reader = new StreamReader(stream)
            
            while not reader.EndOfStream do yield reader.ReadLine()
        }