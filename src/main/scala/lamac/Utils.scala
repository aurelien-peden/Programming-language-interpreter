package lamac

import scala.language.reflectiveCalls

object Utils {
    /** Permet d'utiliser la ressource fermable donnée en argument dans
      * la fonction en deuxième argument, puis la ferme automatiquement
      * (trouvé sur internet)
      */
    def using[A <: {def close(): Unit}, B](resource: A)(f: A => B): B =
        try {
            f(resource)
        } finally {
            resource.close()
        }
}
