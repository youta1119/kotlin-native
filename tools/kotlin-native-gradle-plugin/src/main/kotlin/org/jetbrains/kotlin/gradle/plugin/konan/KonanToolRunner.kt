/*
 * Copyright 2010-2017 JetBrains s.r.o.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 * http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package org.jetbrains.kotlin.gradle.plugin.konan

import org.gradle.api.Named
import org.gradle.api.Project
import org.gradle.api.file.FileCollection
import org.jetbrains.kotlin.konan.target.Family
import org.jetbrains.kotlin.konan.target.HostManager
import org.jetbrains.kotlin.konan.target.KonanTarget
import org.jetbrains.kotlin.konan.util.DependencyProcessor

internal interface KonanToolRunner: Named {
    val mainClass: String
    val classpath: FileCollection
    val jvmArgs: List<String>
    val environment: Map<String, Any>
    val additionalSystemProperties: Map<String, String>

    fun run(args: List<String>)
    fun run(vararg args: String) = run(args.toList())
}

internal abstract class KonanCliRunner(
        val toolName: String,
        val fullName: String,
        val project: Project,
        private val additionalJvmArgs: List<String>
): KonanToolRunner {
    override val mainClass = "org.jetbrains.kotlin.cli.utilities.MainKt"

    override fun getName() = toolName

    // We need to unset some environment variables which are set by XCode and may potentially affect the tool executed.
    protected val blacklistEnvironment: List<String> by lazy {
        KonanPlugin::class.java.getResourceAsStream("/env_blacklist")?.let { stream ->
            stream.reader().use { it.readLines() }
        } ?: emptyList<String>()
    }

    protected val blacklistProperties: Set<String> =
        setOf("java.endorsed.dirs")

    override val classpath: FileCollection =
            project.fileTree("${project.konanHome}/konan/lib/")
            .apply { include("*.jar")  }

    override val jvmArgs = HostManager.defaultJvmArgs.toMutableList().apply {
        if (additionalJvmArgs.none { it.startsWith("-Xmx") } &&
            project.jvmArgs.none { it.startsWith("-Xmx") }) {
            add("-Xmx3G")
        }
        addAll(additionalJvmArgs)
        addAll(project.jvmArgs)
    }

    override val additionalSystemProperties = mutableMapOf(
            "konan.home" to project.konanHome,
            "java.library.path" to "${project.konanHome}/konan/nativelib"
    )

    override val environment = mutableMapOf("LIBCLANG_DISABLE_CRASH_RECOVERY" to "1")

    private fun String.escapeQuotes() = replace("\"", "\\\"")

    private fun Sequence<Pair<String, String>>.escapeQuotesForWindows() =
        if (HostManager.hostIsMingw) {
            map { (key, value) -> key.escapeQuotes() to value.escapeQuotes() }
        } else {
            this
        }

    open protected fun transformArgs(args: List<String>): List<String> = args

    override fun run(args: List<String>) {
        project.logger.info("Run tool: $toolName with args: ${args.joinToString(separator = " ")}")
        if (classpath.isEmpty) {
            throw IllegalStateException("Classpath of the tool is empty: $toolName\n" +
                    "Probably the '${KonanPlugin.ProjectProperty.KONAN_HOME}' project property contains an incorrect path.\n" +
                    "Please change it to the compiler root directory and rerun the build.")
        }

        project.javaexec { spec ->
            spec.main = mainClass
            spec.classpath = classpath
            spec.jvmArgs(jvmArgs)
            spec.systemProperties(
                System.getProperties().asSequence()
                    .map { (k, v) -> k.toString() to v.toString() }
                    .filter { (k, _) -> k !in blacklistProperties }
                    .escapeQuotesForWindows()
                    .toMap()
            )
            spec.systemProperties(additionalSystemProperties)
            spec.args(listOf(toolName) + transformArgs(args))
            blacklistEnvironment.forEach { spec.environment.remove(it) }
            spec.environment(environment)
        }
    }
}

internal class KonanInteropRunner(project: Project, additionalJvmArgs: List<String> = emptyList())
    : KonanCliRunner("cinterop", "Kotlin/Native cinterop tool", project, additionalJvmArgs)
{
    init {
        if (HostManager.host == KonanTarget.MINGW_X64) {
	    //TODO: Oh-ho-ho fix it in more convinient way.
            environment.put("PATH", DependencyProcessor.defaultDependenciesRoot.absolutePath +
                    "\\msys2-mingw-w64-x86_64-gcc-7.3.0-clang-llvm-lld-6.0.1-2" +
                    "\\bin;${environment.get("PATH")}")
        }
    }
}

internal class KonanCompilerRunner(
    project: Project,
    additionalJvmArgs: List<String> = emptyList(),
    val useArgFile: Boolean = true
) : KonanCliRunner("konanc", "Kotlin/Native compiler", project, additionalJvmArgs)
{
    override fun transformArgs(args: List<String>): List<String> {
        if (!useArgFile) {
            return args
        }

        val argFile = createTempFile(prefix = "konancArgs", suffix = ".lst").apply {
            deleteOnExit()
        }
        argFile.printWriter().use { writer ->
            args.forEach {
                writer.println(it)
            }
        }

        return listOf("@${argFile.absolutePath}")
    }
}

internal class KonanKlibRunner(project: Project, additionalJvmArgs: List<String> = emptyList())
    : KonanCliRunner("klib", "Klib management tool", project, additionalJvmArgs)
