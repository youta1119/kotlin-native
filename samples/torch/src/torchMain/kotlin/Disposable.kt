/*
 * Copyright 2010-2018 JetBrains s.r.o. Use of this source code is governed by the Apache 2.0 license
 * that can be found in the license/LICENSE.txt file.
 */

package sample.torch

interface Disposable {
    fun dispose()
}

open class DisposableContainer(private val disposables: MutableList<Disposable> = ArrayList()) : Disposable {
    /**
     * Creates the object and schedules its disposal for the end of the scope.
     */
    fun <T : Disposable> use(create: () -> T) = create().also { disposables.add(it) }

    override fun dispose() {
        for (disposable in disposables) {
            disposable.dispose()
        }
    }
}

fun <T> disposeScoped(action: DisposableContainer.() -> T): T {
    val scope = DisposableContainer()

    try {
        return scope.action()
    } finally {
        scope.dispose()
    }
}